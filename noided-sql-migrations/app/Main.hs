{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.Semigroup (sconcat)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Hasql.Connection (acquire)
import Hasql.Connection qualified as C
import Hasql.Connection.Settings qualified as S
import Noided.Sql.Migration
import Noided.Sql.Migration.Internal
import Noided.Sql.TransactM
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcess)

data Command
  = New FilePath Text
  | Migrate FilePath
  | Rollback FilePath Int

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    New dir name -> handleNew dir name
    Migrate dir -> handleMigrate dir
    Rollback dir n -> handleRollback dir n
  where
    opts =
      info
        (commandParser <**> helper)
        (fullDesc <> progDesc "noided-sql migration tool")

commandParser :: Parser Command
commandParser =
  subparser
    ( command "new" (info (New <$> strOption (long "dir" <> short 'd' <> help "Migrations directory" <> showDefault <> value "db/migrations") <*> argument str (metavar "NAME")) (progDesc "Create a new migration"))
        <> command "migrate" (info (Migrate <$> strOption (long "dir" <> short 'd' <> help "Migrations directory" <> showDefault <> value "db/migrations")) (progDesc "Run pending migrations"))
        <> command "rollback" (info (Rollback <$> strOption (long "dir" <> short 'd' <> help "Migrations directory" <> showDefault <> value "db/migrations") <*> option auto (long "steps" <> short 'n' <> help "Number of migrations to roll back" <> showDefault <> value 1)) (progDesc "Roll back migrations"))
    )

handleNew :: FilePath -> Text -> IO ()
handleNew dir name = do
  now <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y%m%d%H%M" now
  let filenameBase = timestamp <> "_" <> T.unpack (T.replace " " "_" name)
  let filenameUp = filenameBase <> ".up.sql"
  let filenameDown = filenameBase <> ".down.sql"
  createDirectoryIfMissing True dir
  let upPath = dir </> filenameUp
  let downPath = dir </> filenameDown
  TIO.writeFile upPath "-- Write your migration SQL up here\n"
  TIO.writeFile downPath "-- Write your SQL down migration here\n"
  putStrLn $ "Created migration: " <> upPath

handleMigrate :: FilePath -> IO ()
handleMigrate dir = withConnection $ \conn -> do
  migrations <- discoverMigrations dir
  let config = defaultMigrationConfig dir

  res <- runMigrationsInTransactions config migrations conn
  case res of
    SessionErr err -> do
      putStrLn $ "Session error: " <> show err
      exitFailure
    TransactErr () -> do
      putStrLn "Transaction failed"
      exitFailure
    TransactOK () -> do
      putStrLn "Migrations applied successfully"
      dumpSchema dir conn

handleRollback :: FilePath -> Int -> IO ()
handleRollback dir n = withConnection $ \conn -> do
  migrations <- discoverMigrations dir
  let config = defaultMigrationConfig dir

  res <- rollbackMigrationsInTransactions config migrations n conn
  case res of
    SessionErr err -> do
      putStrLn $ "Session error: " <> show err
      exitFailure
    TransactErr () -> do
      putStrLn "Transaction failed"
      exitFailure
    TransactOK () -> do
      putStrLn "Rollback successful"
      dumpSchema dir conn

dumpSchema :: FilePath -> C.Connection -> IO ()
dumpSchema migrationsDir conn = do
  dbUrl <- lookupEnv "DATABASE_URL"
  case dbUrl of
    Just url -> do
      putStrLn "Dumping schema..."
      let config = defaultMigrationConfig migrationsDir

      -- Use pg_dump to get the schema
      let pgDumpArgs = [url, "--schema-only", "--no-owner", "--no-privileges"]
      schema <- readProcess "pg_dump" pgDumpArgs ""

      -- Get applied migrations to generate INSERTS
      appliedRes <- transactSerialized noStatementCallback (getAppliedMigrations config) conn
      let insertStatements = case appliedRes of
            TransactOK applied ->
              if Set.null applied
                then ""
                else
                  "\n-- Applied migrations\n"
                    <> T.unlines ["INSERT INTO " <> trackingTableName config <> " (filename) VALUES ('" <> v <> "');" | v <- Set.toList applied]
            _ -> ""

      -- Filter the schema output
      let filteredSchema = filterSchema schema

      -- Determine the schema.sql path
      let schemaPath = takeDirectory migrationsDir </> "schema.sql"
      TIO.writeFile schemaPath (T.pack filteredSchema <> insertStatements)
      putStrLn $ "Schema dumped to " <> schemaPath
    Nothing -> putStrLn "Skipping schema dump: DATABASE_URL not set"

filterSchema :: String -> String
filterSchema = unlines . filter (not . isGarbage) . lines
  where
    isGarbage line =
      "\\" `isPrefixOf` line
        || "--" `isPrefixOf` line -- Filter out \set, \connect, \restrict, etc.
        || "SET " `isPrefixOf` line -- Filter out comments
        || "SELECT pg_catalog.set_config" `isPrefixOf` line -- Filter out SET statements
        || null (dropWhile (== ' ') line) -- Filter out empty lines

withConnection :: (C.Connection -> IO a) -> IO a
withConnection runAction = do
  dbUrl <- lookupEnv "DATABASE_URL"
  connSettings <- case dbUrl of
    Just url -> pure $ S.connectionString (T.pack url)
    Nothing -> do
      -- Fallback to individual env vars or defaults
      h <- getEnvDefault "DB_HOST" "localhost"
      u <- getEnvDefault "DB_USER" "postgres"
      pw <- getEnvDefault "DB_PASS" ""
      n <- getEnvDefault "DB_NAME" "postgres"
      pure $
        sconcat $
          S.host (T.pack h)
            :| [ S.user (T.pack u),
                 S.password (T.pack pw),
                 S.dbname (T.pack n)
               ]

  connRes <- acquire connSettings
  case connRes of
    Left err -> do
      putStrLn $ "Failed to connect to database: " <> show err
      exitFailure
    Right conn -> runAction conn

getEnvDefault :: String -> String -> IO String
getEnvDefault var def = do
  val <- lookupEnv var
  pure $ Data.Maybe.fromMaybe def val

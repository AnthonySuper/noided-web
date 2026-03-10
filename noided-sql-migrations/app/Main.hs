{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (isPrefixOf)
import Data.Semigroup (sconcat)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
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
import System.FilePath ((</>), takeDirectory)
import System.Process (readProcess)

data Command
  = New Text
  | Migrate FilePath
  | Rollback FilePath Int

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    New name -> handleNew name
    Migrate dir -> handleMigrate dir
    Rollback dir n -> handleRollback dir n
  where
    opts = info (commandParser <**> helper)
      ( fullDesc <> progDesc "noided-sql migration tool" )

commandParser :: Parser Command
commandParser = subparser
  ( command "new" (info (New <$> argument str (metavar "NAME")) (progDesc "Create a new migration"))
  <> command "migrate" (info (Migrate <$> strOption (long "dir" <> short 'd' <> help "Migrations directory" <> showDefault <> value "db/migrations")) (progDesc "Run pending migrations"))
  <> command "rollback" (info (Rollback <$> strOption (long "dir" <> short 'd' <> help "Migrations directory" <> showDefault <> value "db/migrations") <*> option auto (long "steps" <> short 'n' <> help "Number of migrations to roll back" <> showDefault <> value 1)) (progDesc "Roll back migrations"))
  )

handleNew :: Text -> IO ()
handleNew name = do
  now <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y%m%d%H%M" now
  let filename = timestamp <> "_" <> T.unpack (T.replace " " "_" name) <> ".sql"
  let dir = "db/migrations"
  createDirectoryIfMissing True dir
  let path = dir </> filename
  TIO.writeFile path "-- Write your migration SQL here\n"
  putStrLn $ "Created migration: " <> path

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
      dumpSchema dir

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
      dumpSchema dir

dumpSchema :: FilePath -> IO ()
dumpSchema migrationsDir = do
  dbUrl <- lookupEnv "DATABASE_URL"
  case dbUrl of
    Just url -> do
      putStrLn "Dumping schema..."
      -- Use pg_dump to get the schema
      let pgDumpArgs = [url, "--schema-only", "--no-owner", "--no-privileges"]
      schema <- readProcess "pg_dump" pgDumpArgs ""
      
      -- Filter the schema output
      let filteredSchema = filterSchema schema
      
      -- Determine the schema.sql path
      -- If migrationsDir is "optimize-beer/db/migrations", schemaPath should be "optimize-beer/db/schema.sql"
      let schemaPath = takeDirectory migrationsDir </> "schema.sql"
      TIO.writeFile schemaPath (T.pack filteredSchema)
      putStrLn $ "Schema dumped to " <> schemaPath
    Nothing -> putStrLn "Skipping schema dump: DATABASE_URL not set"

filterSchema :: String -> String
filterSchema = unlines . filter (not . isGarbage) . lines
  where
    isGarbage line = 
      "\\" `isPrefixOf` line || -- Filter out \set, \connect, \restrict, etc.
      "--" `isPrefixOf` line || -- Filter out comments
      "SET " `isPrefixOf` line || -- Filter out SET statements
      "SELECT pg_catalog.set_config" `isPrefixOf` line ||
      null (dropWhile (== ' ') line) -- Filter out empty lines

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
      pure $ sconcat $ 
        S.host (T.pack h) :| 
        [ S.user (T.pack u)
        , S.password (T.pack pw)
        , S.dbname (T.pack n)
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
  pure $ case val of
    Just v -> v
    Nothing -> def

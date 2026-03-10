{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.ByteString.Char8 qualified as BS
import Data.Semigroup (sconcat)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Hasql.Connection (acquire)
import Hasql.Connection.Settings qualified as S
import Noided.Sql.Migration
import Noided.Sql.Migration.Internal
import Noided.Sql.TransactM
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))

data Command
  = New Text
  | Migrate FilePath

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    New name -> handleNew name
    Migrate dir -> handleMigrate dir
  where
    opts = info (commandParser <**> helper)
      ( fullDesc <> progDesc "noided-sql migration tool" )

commandParser :: Parser Command
commandParser = subparser
  ( command "new" (info (New <$> argument str (metavar "NAME")) (progDesc "Create a new migration"))
  <> command "migrate" (info (Migrate <$> strOption (long "dir" <> short 'd' <> help "Migrations directory" <> showDefault <> value "db/migrations")) (progDesc "Run pending migrations"))
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
handleMigrate dir = do
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
    Right conn -> do
      migrationFiles <- discoverMigrations dir
      migrations <- mapM readMigration migrationFiles
      let config = defaultMigrationConfig dir
      
      res <- transactSerialized noStatementCallback (runMigrations config migrations) conn
      case res of
        SessionErr err -> do
          putStrLn $ "Session error: " <> show err
          exitFailure
        TransactErr () -> do 
          putStrLn "Transaction failed"
          exitFailure
        TransactOK () -> do
          putStrLn "Migrations applied successfully"

getEnvDefault :: String -> String -> IO String
getEnvDefault var def = do
  val <- lookupEnv var
  pure $ case val of
    Just v -> v
    Nothing -> def

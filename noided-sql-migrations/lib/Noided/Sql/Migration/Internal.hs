{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Noided.Sql.Migration.Internal where

import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))

-- | A migration that is ready to be applied.
data Migration = Migration
  { version :: Text,
    name :: Text,
    content :: Text,
    noTransaction :: Bool
  }
  deriving (Show, Eq)

-- | Information about a migration file found on disk.
data MigrationFile = MigrationFile
  { filePath :: FilePath,
    fileVersion :: Text,
    fileName :: Text
  }
  deriving (Show, Eq)

-- | Configuration for the migration system.
data MigrationConfig = MigrationConfig
  { migrationsDirectory :: FilePath,
    trackingTableName :: Text
  }
  deriving (Show, Eq)

defaultMigrationConfig :: FilePath -> MigrationConfig
defaultMigrationConfig dir =
  MigrationConfig
    { migrationsDirectory = dir,
      trackingTableName = "schema_migrations"
    }

-- | Discover and sort migration files in a directory.
-- Expects files in the format: YYYYMMDDHHMM_name.sql
discoverMigrations :: FilePath -> IO [MigrationFile]
discoverMigrations dir = do
  files <- listDirectory dir
  let sqlFiles = filter (\f -> takeExtension f == ".sql") files
  let migrationFiles = map (parseMigrationFile . (dir </>)) sqlFiles
  pure $ sortOn fileVersion migrationFiles

-- | Parse a file path into a 'MigrationFile'.
parseMigrationFile :: FilePath -> MigrationFile
parseMigrationFile path =
  let baseName = T.pack $ takeBaseName path
      (versionPart, namePart) = T.breakOn "_" baseName
   in MigrationFile
        { filePath = path,
          fileVersion = versionPart,
          fileName = T.drop 1 namePart -- Remove the leading underscore
        }

-- | Read the content of a migration file.
readMigration :: MigrationFile -> IO Migration
readMigration MigrationFile {..} = do
  content <- TIO.readFile filePath
  let noTx = "-- no-transaction" `T.isPrefixOf` T.stripStart content
  pure $
    Migration
      { version = fileVersion,
        name = fileName,
        content = content,
        noTransaction = noTx
      }

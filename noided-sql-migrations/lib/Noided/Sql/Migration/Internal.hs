{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Noided.Sql.Migration.Internal where

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Monad (foldM)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))

-- | The direction of a migration.
data MigrationDirection = Up | Down
  deriving (Show, Eq, Ord)

-- | A migration that is ready to be applied or rolled back.
data Migration = Migration
  { version :: Text,
    name :: Text,
    upContent :: Text,
    upNoTransaction :: Bool,
    downContent :: Maybe Text,
    downNoTransaction :: Bool
  }
  deriving (Show, Eq)

-- | Information about migration files found on disk for a single version.
data MigrationFiles = MigrationFiles
  { mVersion :: Text,
    mName :: Text,
    mUpPath :: Maybe FilePath,
    mDownPath :: Maybe FilePath
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

-- | Discover and sort migrations in a directory.
-- Groups files by version and name.
-- Fails with an error if duplicate UP or DOWN files are found for the same version.
discoverMigrations :: FilePath -> IO [Migration]
discoverMigrations dir = do
  files <- listDirectory dir
  let sqlFiles = filter (\f -> takeExtension f == ".sql") files
  let rawFiles = mapMaybe (parseRawMigrationFile . (dir </>)) sqlFiles
  let pairs = map (\r -> ((rVersion r, rName r), parseRawToFiles r)) rawFiles

  grouped <- foldM insertFile Map.empty pairs
  let migrationFiles = Map.elems grouped

  mapM readMigration $ sortOn mVersion migrationFiles
  where
    insertFile acc (key, mf) =
      case Map.lookup key acc of
        Nothing -> pure $ Map.insert key mf acc
        Just existing -> do
          merged <- mergeFiles existing mf
          pure $ Map.insert key merged acc

    mergeFiles f1 f2 = do
      upPath <- checkDuplicate "UP" (mVersion f1) (mUpPath f1) (mUpPath f2)
      downPath <- checkDuplicate "DOWN" (mVersion f1) (mDownPath f1) (mDownPath f2)
      pure MigrationFiles
        { mVersion = mVersion f1
        , mName = mName f1
        , mUpPath = upPath
        , mDownPath = downPath
        }

    checkDuplicate direction version (Just p1) (Just p2) =
      throwIO $ userError $
        "Duplicate " <> direction <> " migration file for version "
          <> T.unpack version <> ": " <> p1 <> " and " <> p2
    checkDuplicate _ _ p1 p2 = pure (p1 <|> p2)

-- | Raw information about a single migration file.
data RawMigrationFile = RawMigrationFile
  { rVersion :: Text,
    rName :: Text,
    rDirection :: MigrationDirection,
    rPath :: FilePath
  }

-- | Parse a file path into a 'RawMigrationFile'.
parseRawMigrationFile :: FilePath -> Maybe RawMigrationFile
parseRawMigrationFile path =
  let fullBaseName = T.pack $ takeBaseName path
      (baseName, direction) = 
        if ".up" `T.isSuffixOf` fullBaseName
          then (T.dropEnd 3 fullBaseName, Up)
          else if ".down" `T.isSuffixOf` fullBaseName
                 then (T.dropEnd 5 fullBaseName, Down)
                 else (fullBaseName, Up)
      (versionPart, namePart) = T.breakOn "_" baseName
   in if T.null namePart
        then Nothing
        else Just RawMigrationFile
               { rVersion = versionPart,
                 rName = T.drop 1 namePart, -- Remove the leading underscore
                 rDirection = direction,
                 rPath = path
               }

-- | Convert a 'RawMigrationFile' into 'MigrationFiles'.
parseRawToFiles :: RawMigrationFile -> MigrationFiles
parseRawToFiles RawMigrationFile{..} =
  case rDirection of
    Up -> MigrationFiles rVersion rName (Just rPath) Nothing
    Down -> MigrationFiles rVersion rName Nothing (Just rPath)

-- | Read the content of paired migration files.
readMigration :: MigrationFiles -> IO Migration
readMigration MigrationFiles {..} = do
  (upContent, upNoTx) <- case mUpPath of
    Just path -> do
      c <- TIO.readFile path
      pure (c, "-- no-transaction" `T.isPrefixOf` T.stripStart c)
    Nothing -> error $ "Missing UP migration for version " <> T.unpack mVersion
    
  (downContent, downNoTx) <- case mDownPath of
    Just path -> do
      c <- TIO.readFile path
      pure (Just c, "-- no-transaction" `T.isPrefixOf` T.stripStart c)
    Nothing -> pure (Nothing, False)

  pure $
    Migration
      { version = mVersion,
        name = mName,
        upContent = upContent,
        upNoTransaction = upNoTx,
        downContent = downContent,
        downNoTransaction = downNoTx
      }

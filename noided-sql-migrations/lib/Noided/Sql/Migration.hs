{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Noided.Sql.Migration where

import Control.Monad (forM_)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as V
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Noided.Sql.Internal.Type.SqlQuery (SqlQuery(..), unsafeQueryFromScript)
import Noided.Sql.Internal.Type.TransactM (execQueryRaw)
import Noided.Sql.Migration.Internal
import Noided.Sql.TransactM

-- | Run pending migrations.
-- This function takes the migrations already read from disk.
runMigrations :: MigrationConfig -> [Migration] -> TransactM err ()
runMigrations config migrations = do
  ensureMigrationTable config
  applied <- getAppliedMigrations config
  
  let pending = filter (\m -> not (Set.member (version m) applied)) migrations
  
  forM_ pending $ \migration -> do
    applyMigration config migration

-- | Ensure the tracking table exists.
ensureMigrationTable :: MigrationConfig -> TransactM err ()
ensureMigrationTable MigrationConfig{..} =
  execQueryRaw $ UnsafeSqlQ
    { syntax = "CREATE TABLE IF NOT EXISTS " <> trackingTableName <> " (filename TEXT PRIMARY KEY);"
    , paramsInspected = []
    , params = Enc.noParams
    , decoder = Dec.noResult
    }

-- | Get the set of versions that have already been applied.
getAppliedMigrations :: MigrationConfig -> TransactM err (Set Text)
getAppliedMigrations MigrationConfig{..} = do
  versions <- execQueryRaw $ UnsafeSqlQ
    { syntax = "SELECT filename FROM " <> trackingTableName
    , paramsInspected = []
    , params = Enc.noParams
    , decoder = Dec.rowVector (Dec.column (Dec.nonNullable Dec.text))
    }
  pure $ Set.fromList (V.toList versions)

-- | Run a single migration and record it in the tracking table.
applyMigration :: MigrationConfig -> Migration -> TransactM err ()
applyMigration MigrationConfig{..} Migration{..} = do
  -- 1. Execute the migration SQL
  execQueryRaw $ unsafeQueryFromScript content
  
  -- 2. Record the migration in the tracking table
  execQueryRaw $ unsafeQueryFromScript $ 
    "INSERT INTO " <> trackingTableName <> " (filename) VALUES ('" <> version <> "');"

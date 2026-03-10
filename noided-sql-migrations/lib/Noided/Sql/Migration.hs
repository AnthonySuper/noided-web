{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Noided.Sql.Migration where

import Control.Monad (foldM, unless, void, when)
import Data.Char (ord)
import Data.Int (Int64)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Hasql.Connection qualified as C
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Noided.Sql.Internal.Type.SqlQuery (SqlQuery (..), unsafeQueryFromScript)
import Noided.Sql.Internal.Type.TransactM (execQueryRaw)
import Noided.Sql.Migration.Internal
import Noided.Sql.TransactM

-- | Run pending migrations, each in its own transaction.
-- This function uses a PostgreSQL advisory lock to ensure that only one
-- migration process runs at a time for a given tracking table.
runMigrationsInTransactions :: MigrationConfig -> [Migration] -> C.Connection -> IO (TransactionResult err ())
runMigrationsInTransactions config migrations conn = do
  -- 1. Acquire advisory lock (session-level)
  lockRes <- transactSerialized noStatementCallback (acquireAdvisoryLock config) conn
  case lockRes of
    TransactOK () -> do
      -- 2. Run migrations logic
      res <- runMigrationsLogic config migrations conn
      -- 3. Release advisory lock
      _ <- transactSerialized noStatementCallback (releaseAdvisoryLock config) conn
      pure res
    err -> pure (void err)

runMigrationsLogic :: MigrationConfig -> [Migration] -> C.Connection -> IO (TransactionResult err ())
runMigrationsLogic config migrations conn = do
  appliedRes <-
    transactSerialized
      noStatementCallback
      ( do
          ensureMigrationTable config
          getAppliedMigrations config
      )
      conn

  case appliedRes of
    TransactOK applied -> do
      let pending = filter (\m -> not (Set.member (version m) applied)) migrations
      foldM
        ( \acc migration ->
            case acc of
              TransactOK () ->
                if upNoTransaction migration
                  then unsafeFakeTransaction noStatementCallback (applyMigration config migration) conn
                  else transactSerialized noStatementCallback (applyMigration config migration) conn
              _ -> pure acc
        )
        (TransactOK ())
        pending
    SessionErr e -> pure $ SessionErr e
    TransactErr e -> pure $ TransactErr e

-- | Roll back the last N migrations.
rollbackMigrationsInTransactions :: MigrationConfig -> [Migration] -> Int -> C.Connection -> IO (TransactionResult err ())
rollbackMigrationsInTransactions config migrations n conn = do
  -- 1. Acquire advisory lock (session-level)
  lockRes <- transactSerialized noStatementCallback (acquireAdvisoryLock config) conn
  case lockRes of
    TransactOK () -> do
      -- 2. Run rollback logic
      res <- rollbackMigrationLogic config migrations n conn
      -- 3. Release advisory lock
      _ <- transactSerialized noStatementCallback (releaseAdvisoryLock config) conn
      pure res
    err -> pure (void err)

rollbackMigrationLogic :: MigrationConfig -> [Migration] -> Int -> C.Connection -> IO (TransactionResult err ())
rollbackMigrationLogic config migrations n conn = do
  appliedRes <-
    transactSerialized
      noStatementCallback
      ( do
          ensureMigrationTable config
          getAppliedMigrations config
      )
      conn

  case appliedRes of
    TransactOK applied -> do
      -- We only want to roll back migrations that ARE applied, in reverse order
      let toRollback = take n $ reverse $ filter (\m -> Set.member (version m) applied) migrations
      foldM
        ( \acc migration ->
            case acc of
              TransactOK () ->
                if downNoTransaction migration
                  then unsafeFakeTransaction noStatementCallback (rollbackMigration config migration) conn
                  else transactSerialized noStatementCallback (rollbackMigration config migration) conn
              _ -> pure acc
        )
        (TransactOK ())
        toRollback
    SessionErr e -> pure $ SessionErr e
    TransactErr e -> pure $ TransactErr e

-- | Acquire a session-level advisory lock based on the tracking table name.
acquireAdvisoryLock :: MigrationConfig -> TransactM err ()
acquireAdvisoryLock MigrationConfig {..} =
  execQueryRaw $
    UnsafeSqlQ
      { syntax = "SELECT pg_advisory_lock(" <> T.pack (show (lockId trackingTableName)) <> ");",
        paramsInspected = [],
        params = Enc.noParams,
        decoder = Dec.noResult
      }

-- | Release a session-level advisory lock.
releaseAdvisoryLock :: MigrationConfig -> TransactM err ()
releaseAdvisoryLock MigrationConfig {..} =
  execQueryRaw $
    UnsafeSqlQ
      { syntax = "SELECT pg_advisory_unlock(" <> T.pack (show (lockId trackingTableName)) <> ");",
        paramsInspected = [],
        params = Enc.noParams,
        decoder = Dec.noResult
      }

-- | Generate a stable 64-bit lock ID from the tracking table name.
lockId :: Text -> Int64
lockId = T.foldl' (\h c -> 31 * h + fromIntegral (ord c)) 82395162

-- | Ensure the tracking table exists.
ensureMigrationTable :: MigrationConfig -> TransactM err ()
ensureMigrationTable MigrationConfig {..} =
  execQueryRaw $
    UnsafeSqlQ
      { syntax = "CREATE TABLE IF NOT EXISTS " <> trackingTableName <> " (filename TEXT PRIMARY KEY);",
        paramsInspected = [],
        params = Enc.noParams,
        decoder = Dec.noResult
      }

-- | Get the set of versions that have already been applied.
getAppliedMigrations :: MigrationConfig -> TransactM err (Set Text)
getAppliedMigrations MigrationConfig {..} = do
  versions <-
    execQueryRaw $
      UnsafeSqlQ
        { syntax = "SELECT filename FROM " <> trackingTableName,
          paramsInspected = [],
          params = Enc.noParams,
          decoder = Dec.rowVector (Dec.column (Dec.nonNullable Dec.text))
        }
  pure $ Set.fromList (V.toList versions)

-- | Run a single migration and record it in the tracking table.
-- This function is idempotent: it checks if the migration has already been applied.
applyMigration :: MigrationConfig -> Migration -> TransactM err ()
applyMigration config@MigrationConfig {..} Migration {..} = do
  alreadyApplied <- isMigrationApplied config version
  unless alreadyApplied $ do
    -- 1. Execute the migration SQL
    execQueryRaw $ unsafeQueryFromScript upContent

    -- 2. Record the migration in the tracking table
    execQueryRaw $
      UnsafeSqlQ
        { syntax = "INSERT INTO " <> trackingTableName <> " (filename) VALUES ('" <> version <> "');",
          paramsInspected = [],
          params = Enc.noParams,
          decoder = Dec.noResult
        }

-- | Roll back a single migration and remove it from the tracking table.
rollbackMigration :: MigrationConfig -> Migration -> TransactM err ()
rollbackMigration config@MigrationConfig {..} Migration {..} = do
  alreadyApplied <- isMigrationApplied config version
  when alreadyApplied $ do
    -- 1. Execute the migration SQL (if provided)
    case downContent of
      Just content -> execQueryRaw $ unsafeQueryFromScript content
      Nothing -> pure ()

    -- 2. Remove the migration from the tracking table
    execQueryRaw $
      UnsafeSqlQ
        { syntax = "DELETE FROM " <> trackingTableName <> " WHERE filename = '" <> version <> "';",
          paramsInspected = [],
          params = Enc.noParams,
          decoder = Dec.noResult
        }

-- | Check if a specific migration version has already been applied.
isMigrationApplied :: MigrationConfig -> Text -> TransactM err Bool
isMigrationApplied MigrationConfig {..} v = do
  res <-
    execQueryRaw $
      UnsafeSqlQ
        { syntax = "SELECT 1 FROM " <> trackingTableName <> " WHERE filename = '" <> v <> "';",
          paramsInspected = [],
          params = Enc.noParams,
          decoder = Dec.rowVector (Dec.column (Dec.nonNullable Dec.int4))
        }
  pure $ not (V.null res)

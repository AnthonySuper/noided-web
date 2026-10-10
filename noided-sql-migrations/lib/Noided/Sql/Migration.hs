{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Noided.Sql.Migration where

import Control.Monad (unless, when)
import Control.Monad.Except (catchError, throwError)
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
import Hasql.Errors (SessionError)
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Noided.Sql.Migration.Internal

-- | Quote a SQL identifier (e.g. table name) using PostgreSQL double-quote syntax.
-- All embedded double-quote characters are escaped by doubling them.
-- This prevents SQL injection when embedding identifiers in queries.
quoteIdentifier :: Text -> Text
quoteIdentifier t = "\"" <> T.replace "\"" "\"\"" t <> "\""

-- | Run a 'Session' inside @BEGIN; … COMMIT;@.
-- If it fails with a 'SessionError', send @ROLLBACK@ (ignoring any error from doing so) and rethrow the original error.
--
-- Plain @BEGIN@ is enough here: the session-level advisory lock already ensures only one migration process runs at a time.
inTransaction :: Session a -> Session a
inTransaction sess = do
  Session.script "BEGIN;"
  (sess <* Session.script "COMMIT;") `catchError` \e -> do
    Session.script "ROLLBACK;" `catchError` \_ -> pure ()
    throwError e

-- | Run each session on the connection in order, stopping at the first failure.
runUntilFailure :: C.Connection -> [Session ()] -> IO (Either SessionError ())
runUntilFailure _ [] = pure (Right ())
runUntilFailure conn (sess : rest) = do
  res <- C.use conn sess
  case res of
    Left e -> pure (Left e)
    Right () -> runUntilFailure conn rest

-- | Run an action while holding the advisory lock for the tracking table.
withAdvisoryLock :: MigrationConfig -> C.Connection -> IO (Either SessionError ()) -> IO (Either SessionError ())
withAdvisoryLock config conn action = do
  -- 1. Acquire advisory lock (session-level)
  lockRes <- C.use conn (inTransaction (acquireAdvisoryLock config))
  case lockRes of
    Left e -> pure (Left e)
    Right () -> do
      -- 2. Run the action
      res <- action
      -- 3. Release advisory lock
      _ <- C.use conn (inTransaction (releaseAdvisoryLock config))
      pure res

-- | Run pending migrations, each in its own transaction.
-- This function uses a PostgreSQL advisory lock to ensure that only one
-- migration process runs at a time for a given tracking table.
runMigrationsInTransactions :: MigrationConfig -> [Migration] -> C.Connection -> IO (Either SessionError ())
runMigrationsInTransactions config migrations conn =
  withAdvisoryLock config conn (runMigrationsLogic config migrations conn)

runMigrationsLogic :: MigrationConfig -> [Migration] -> C.Connection -> IO (Either SessionError ())
runMigrationsLogic config migrations conn = do
  appliedRes <- C.use conn (inTransaction (ensureMigrationTable config *> getAppliedMigrations config))
  case appliedRes of
    Left e -> pure (Left e)
    Right applied -> do
      let pending = filter (\m -> not (Set.member (migrationId m) applied)) migrations
      runUntilFailure conn $
        [ if upNoTransaction migration
            then applyMigration config migration
            else inTransaction (applyMigration config migration)
        | migration <- pending
        ]

-- | Roll back the last N migrations.
rollbackMigrationsInTransactions :: MigrationConfig -> [Migration] -> Int -> C.Connection -> IO (Either SessionError ())
rollbackMigrationsInTransactions config migrations n conn =
  withAdvisoryLock config conn (rollbackMigrationLogic config migrations n conn)

rollbackMigrationLogic :: MigrationConfig -> [Migration] -> Int -> C.Connection -> IO (Either SessionError ())
rollbackMigrationLogic config migrations n conn = do
  appliedRes <- C.use conn (inTransaction (ensureMigrationTable config *> getAppliedMigrations config))
  case appliedRes of
    Left e -> pure (Left e)
    Right applied -> do
      -- We only want to roll back migrations that ARE applied, in reverse order
      let toRollback = take n $ reverse $ filter (\m -> Set.member (migrationId m) applied) migrations
      runUntilFailure conn $
        [ if downNoTransaction migration
            then rollbackMigration config migration
            else inTransaction (rollbackMigration config migration)
        | migration <- toRollback
        ]

-- | Run a hand-written SQL statement.
-- It is unpreparable because the tracking table name is spliced into the SQL via 'quoteIdentifier'.
runStatement :: params -> Text -> Enc.Params params -> Dec.Result result -> Session result
runStatement params sql encoder decoder = Session.statement params (Statement.unpreparable sql encoder decoder)

-- | Acquire a session-level advisory lock based on the tracking table name.
acquireAdvisoryLock :: MigrationConfig -> Session ()
acquireAdvisoryLock MigrationConfig {..} =
  runStatement
    ()
    ("SELECT pg_advisory_lock(" <> T.pack (show (lockId trackingTableName)) <> ");")
    Enc.noParams
    Dec.noResult

-- | Release a session-level advisory lock.
releaseAdvisoryLock :: MigrationConfig -> Session ()
releaseAdvisoryLock MigrationConfig {..} =
  runStatement
    ()
    ("SELECT pg_advisory_unlock(" <> T.pack (show (lockId trackingTableName)) <> ");")
    Enc.noParams
    Dec.noResult

-- | Generate a stable 64-bit lock ID from the tracking table name.
lockId :: Text -> Int64
lockId = T.foldl' (\h c -> 31 * h + fromIntegral (ord c)) 82395162

-- | Ensure the tracking table exists.
ensureMigrationTable :: MigrationConfig -> Session ()
ensureMigrationTable MigrationConfig {..} =
  runStatement
    ()
    ("CREATE TABLE IF NOT EXISTS " <> quoteIdentifier trackingTableName <> " (filename TEXT PRIMARY KEY);")
    Enc.noParams
    Dec.noResult

-- | Get the set of versions that have already been applied.
getAppliedMigrations :: MigrationConfig -> Session (Set Text)
getAppliedMigrations MigrationConfig {..} = do
  versions <-
    runStatement
      ()
      ("SELECT filename FROM " <> quoteIdentifier trackingTableName)
      Enc.noParams
      (Dec.rowVector (Dec.column (Dec.nonNullable Dec.text)))
  pure $ Set.fromList (V.toList versions)

-- | Run a single migration and record it in the tracking table.
-- This function is idempotent: it checks if the migration has already been applied.
applyMigration :: MigrationConfig -> Migration -> Session ()
applyMigration config@MigrationConfig {..} migration@Migration {..} = do
  alreadyApplied <- isMigrationApplied config (migrationId migration)
  unless alreadyApplied $ do
    -- 1. Execute the migration SQL
    Session.script upContent

    -- 2. Record the migration in the tracking table
    runStatement
      (migrationId migration)
      ("INSERT INTO " <> quoteIdentifier trackingTableName <> " (filename) VALUES ($1);")
      (Enc.param (Enc.nonNullable Enc.text))
      Dec.noResult

-- | Roll back a single migration and remove it from the tracking table.
rollbackMigration :: MigrationConfig -> Migration -> Session ()
rollbackMigration config@MigrationConfig {..} migration@Migration {..} = do
  alreadyApplied <- isMigrationApplied config (migrationId migration)
  when alreadyApplied $ do
    -- 1. Execute the migration SQL (if provided)
    case downContent of
      Just content -> Session.script content
      Nothing -> pure ()

    -- 2. Remove the migration from the tracking table
    runStatement
      (migrationId migration)
      ("DELETE FROM " <> quoteIdentifier trackingTableName <> " WHERE filename = $1;")
      (Enc.param (Enc.nonNullable Enc.text))
      Dec.noResult

-- | Check if a specific migration version has already been applied.
isMigrationApplied :: MigrationConfig -> Text -> Session Bool
isMigrationApplied MigrationConfig {..} v = do
  res <-
    runStatement
      v
      ("SELECT 1 FROM " <> quoteIdentifier trackingTableName <> " WHERE filename = $1;")
      (Enc.param (Enc.nonNullable Enc.text))
      (Dec.rowVector (Dec.column (Dec.nonNullable Dec.int4)))
  pure $ not (V.null res)

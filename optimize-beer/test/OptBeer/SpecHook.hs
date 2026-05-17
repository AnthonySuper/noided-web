{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module OptBeer.SpecHook (hook) where

import Control.Exception (Exception, throwIO)
import Data.Pool (Pool, defaultPoolConfig, newPool)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Hasql.Connection (Connection, acquire, release)
import Hasql.Connection.Settings (connectionString)
import Hasql.Errors (ConnectionError)
import Noided.Sql.Migration
import Noided.Sql.Migration.Internal
import Noided.Sql.TransactM (TransactionResult (..))
import Test.Hspec
import TestContainers qualified as TC
import TestContainers.Config qualified as TC
import TestContainers.Hspec qualified as TC
import TestContainers.Monad qualified as TC

data SpecHookError
  = ConnectionAcquisitionFailed ConnectionError
  | MigrationFailed
  deriving (Show)

instance Exception SpecHookError

hook :: SpecWith (Pool Connection) -> Spec
hook s = withTestContainers (withConnectionPool (parallel s))

spinUpContainers :: TC.TestContainer (T.Text, TC.Container)
spinUpContainers = do
  c <-
    TC.run $
      TC.containerRequest (TC.fromTag "postgres:18-alpine")
        TC.& TC.setExpose [5432]
        TC.& TC.setWaitingFor waitUntilReady
        TC.& TC.setEnv [("POSTGRES_PASSWORD", "postgres")]
  let port = TC.containerPort c 5432
  let connStr = "postgres://postgres:postgres@localhost:" <> T.pack (show port) <> "/postgres"

  pure (connStr, c)
  where
    waitUntilReady :: TC.WaitUntilReady
    waitUntilReady =
      mconcat
        [ TC.waitForLogLine TC.Stderr (LT.isInfixOf "database system is ready to accept connections"),
          TC.waitUntilMappedPortReachable 5432
        ]

withTestContainers :: SpecWith (T.Text, TC.Container) -> Spec
withTestContainers = aroundAll (TC.withContainers spinUpContainers)

withConnectionPool :: SpecWith (Pool Connection) -> SpecWith (T.Text, b)
withConnectionPool = beforeAllWith $ \(url, _container) -> do
  -- Apply migrations
  putStrLn $ "Applying migrations to test container at " <> T.unpack url <> "..."
  let dir = "db/migrations" -- Relative to optimize-beer root
  migrations <- discoverMigrations dir
  let config = defaultMigrationConfig dir

  connRes <- acquire (connectionString url)
  case connRes of
    Left err -> throwIO $ ConnectionAcquisitionFailed err
    Right conn -> do
      res <- runMigrationsInTransactions config migrations conn :: IO (TransactionResult String ())
      release conn
      case res of
        TransactOK () -> putStrLn "Migrations applied successfully"
        _ -> do
          putStrLn $ "Migration failed: " <> show res
          throwIO MigrationFailed

  newPool $
    defaultPoolConfig
      (acquire (connectionString url) >>= either (throwIO . ConnectionAcquisitionFailed) return)
      release
      30.0 -- Keep resources for 30 seconds
      10 -- 10 resources per stripe

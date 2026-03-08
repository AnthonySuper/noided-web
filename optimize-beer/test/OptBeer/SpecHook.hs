{-# LANGUAGE OverloadedStrings #-}

module OptBeer.SpecHook (hook) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.List (nubBy)
import Data.Pool (Pool, defaultPoolConfig, newPool)
import Data.Text qualified as T
import Hasql.Connection
import Hasql.Connection.Settings (connectionString)
import Hasql.Errors
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Test.Hspec
import TestContainers qualified as TC
import TestContainers.Hspec (withContainers)

data SpecHookError
  = ConnectionAcquisitionFailed ConnectionError
  | MigrationFailed Int String -- ^ exit code and combined stdout+stderr
  deriving (Show)

instance Exception SpecHookError

hook :: SpecWith (Pool Connection) -> Spec
hook s = withConnectionPool (parallel s)

-- | A spec hook that gets you a connection pool backed by a fresh PostgreSQL
-- container.  The container is started once for the entire test suite and
-- torn down automatically afterwards.
withConnectionPool :: SpecWith (Pool Connection) -> Spec
withConnectionPool = aroundAll (withContainers setupContainer)

-- | Spin up a PostgreSQL 18 container, run migrations against it, and return
-- a connection pool pointing at the containerised database.
setupContainer :: TC.TestContainer (Pool Connection)
setupContainer = do
  pgContainer <-
    TC.run $
      TC.containerRequest (TC.fromTag "postgres:18")
        TC.& TC.setEnv
          [ ("POSTGRES_DB", "optimize_beer_test"),
            ("POSTGRES_USER", "postgres"),
            ("POSTGRES_PASSWORD", "password")
          ]
        TC.& TC.setExpose [5432]
        TC.& TC.setWaitingFor (TC.waitUntilMappedPortReachable 5432)
  let port = TC.containerPort pgContainer 5432
      dbUrl = "postgresql://postgres:password@localhost:" <> show port <> "/optimize_beer_test"
  liftIO $ runMigrations dbUrl
  liftIO $
    newPool $
      defaultPoolConfig
        (acquire (connectionString (T.pack dbUrl)) >>= either (throwIO . ConnectionAcquisitionFailed) return)
        release
        30.0 -- Keep resources for 30 seconds
        10 -- 10 resources per stripe

-- | Shell out to the Ruby migration script, pointing it at the given
-- @DATABASE_URL@.  Paths are relative to the package root (@optimize-beer/@),
-- which is the working directory used by @cabal test@.
runMigrations :: String -> IO ()
runMigrations dbUrl = do
  currentEnv <- getEnvironment
  let overrides =
        [ ("DATABASE_URL", dbUrl),
          ("OPTIMIZE_BEER_ENV", "test"),
          -- Point bundler at the Gemfile in the package root.
          ("BUNDLE_GEMFILE", "Gemfile")
        ]
      -- New values first; nubBy keeps the first occurrence per key, so the
      -- overrides take precedence over any identically-named vars inherited
      -- from the parent process environment.
      mergedEnv = nubBy ((==) `on` fst) (overrides ++ currentEnv)
      -- Script path is relative to the package root (optimize-beer/).
      processSpec = (proc "ruby" ["script/db.rb", "migrate"]) {env = Just mergedEnv}
  (exitCode, out, err) <- readCreateProcessWithExitCode processSpec ""
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure n -> throwIO (MigrationFailed n (out <> err))

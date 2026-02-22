{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module OptBeer.SpecHook (hook) where

import Control.Arrow
import Control.Exception (Exception, throwIO)
import Data.Pool (Pool, defaultPoolConfig, newPool)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import Hasql.Connection
import Hasql.Connection.Settings (connectionString)
import Hasql.Errors
import Noided.Web.ApplicationConfig
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Test.Hspec

data SpecHookError
  = DatabaseUrlNotSetAndConfigMissing FilePath
  | TestConfigMissing FilePath
  | ConnectionAcquisitionFailed ConnectionError
  deriving (Show)

instance Exception SpecHookError

hook :: SpecWith (Pool Connection) -> Spec
hook s = withConnectionPool (parallel s)

-- | A spec hook that gets you a connection pool.
withConnectionPool :: SpecWith (Pool Connection) -> Spec
withConnectionPool = beforeAll $ do
  mUrl <- lookupEnv "DATABASE_URL"
  settings <- case mUrl of
    Just url -> return $ connectionString (T.pack url)
    Nothing -> do
      let dbFile = "config/db.yml"
      exists <- doesFileExist dbFile
      if not exists
        then throwIO $ DatabaseUrlNotSetAndConfigMissing dbFile
        else do
          cfg <- Yaml.decodeFileThrow dbFile :: IO DBFileConfig
          case cfg.test of
            Nothing -> throwIO $ TestConfigMissing dbFile
            Just s -> return $ getHasqlSettings s

  newPool $
    defaultPoolConfig
      (acquire settings >>= either (throwIO . ConnectionAcquisitionFailed) return)
      release
      30.0 -- Keep resources for 30 seconds
      10 -- 10 resources per stripe

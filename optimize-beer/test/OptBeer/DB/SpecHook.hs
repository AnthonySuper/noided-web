{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module OptBeer.DB.SpecHook (hook) where

import Control.Exception (Exception, throwIO)
import Data.Pool (Pool, defaultPoolConfig, newPool)
import Hasql.Connection
import Hasql.Errors
import Hasql.Connection.Settings (connectionString)
import Noided.Web.Internal.Type.DBSettings
import System.Environment (lookupEnv)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import Test.Hspec
import System.Directory (doesFileExist)

data SpecHookError
  = DatabaseUrlNotSetAndConfigMissing FilePath
  | TestConfigMissing FilePath
  | ConnectionAcquisitionFailed ConnectionError
  deriving (Show)

instance Exception SpecHookError

-- | A spec hook that gets you a connection pool.
hook :: SpecWith (Pool Connection) -> Spec
hook = beforeAll $ do
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

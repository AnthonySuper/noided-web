module Noided.Web.Internal.Type.ApplicationConfig where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.List (isPrefixOf)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.IO qualified as T
import Data.Yaml qualified as Yaml
import GHC.Generics
import Hasql.Connection.Settings (connectionString)
import Noided.Web.Internal.Type.DBSettings
import Noided.Web.Internal.Type.ServerEnv
import Noided.Web.Internal.Type.SignedMessage (Signer, signerHMACSHA512)
import System.Directory
import System.Environment
import System.FilePath

data ApplicationConfig
  = AppCfg
  { -- | The server environment.
    serverEnv :: ServerEnv,
    -- | The database settings.
    -- When configuration is read in the @Development@ and @Test@ modes,
    -- this is picked up from the file @config/db.yml@.
    --
    -- When configuration is read in the @Production@ mode, this is picked up
    -- from the env variable @DATABASE_URL@.
    dbSettings :: DBSettings,
    -- | The primary signer of the application.
    -- When configuration is read in @Development@ mode, this is read from the file
    -- @config/signers/development.txt@.
    -- When configuration is read in the @Test@ mode, this is read from the file
    -- @config/signers/test.txt@.
    -- When configuration is read in the @Production@ mode, this is read from the
    -- @PRIMARY_SIGNING_KEY@ environment variable.
    primarySigner :: Signer,
    -- | Possible secondary signers for the app.
    -- When the configuration is read in @Development@ mode, this is read from any file
    -- matching @config/signers/development-backup-*.txt@.
    -- When the configuration is read in the @Test@ mode, this is read from any file
    -- matching @config/signers/test-backup-*.txt@.
    -- When the configuration is read in the @Production@ mode, this is read from any env var
    -- whose key has the prefix of @BACKUP_SIGNING_KEY_@.
    secondarySigners :: [Signer]
  }
  deriving (Generic)

-- | Possible reasons why configuration failed.
data ConfigurationReadFailedError
  = NoDBSettingsDirectory
  | DBSettingsFailedParse String
  | DBSettingsFileNotFound FilePath
  | PrimarySignerFileNotFound FilePath
  | EnvironmentVariableMissing String
  | SignerFileReadFailed FilePath String
  deriving (Show, Generic)

instance Exception ConfigurationReadFailedError

readConfigurationForEnv :: ServerEnv -> IO (Either ConfigurationReadFailedError ApplicationConfig)
readConfigurationForEnv se = runExceptT $ do
  dbS <- case se of
    Production -> do
      url <-
        liftIO (lookupEnv "DATABASE_URL") >>= \case
          Nothing -> throwE $ EnvironmentVariableMissing "DATABASE_URL"
          Just u -> return $ T.pack u
      return $ DBSettings $ connectionString url
    _ -> do
      let dbFile = "config/db.yml"
      exists <- liftIO $ doesFileExist dbFile
      unless exists $ throwE $ DBSettingsFileNotFound dbFile
      cfg <-
        liftIO (Yaml.decodeFileEither dbFile) >>= \case
          Left err -> throwE $ DBSettingsFailedParse (show err)
          Right c -> return (c :: DBFileConfig)
      case se of
        Development -> case cfg.development of
          Nothing -> throwE $ DBSettingsFailedParse "No development settings in config/db.yml"
          Just s -> return s
        Test -> case cfg.test of
          Nothing -> throwE $ DBSettingsFailedParse "No test settings in config/db.yml"
          Just s -> return s

  (primary, secondaries) <- case se of
    Production -> do
      pKey <-
        liftIO (lookupEnv "PRIMARY_SIGNING_KEY") >>= \case
          Nothing -> throwE $ EnvironmentVariableMissing "PRIMARY_SIGNING_KEY"
          Just k -> return $ TEnc.encodeUtf8 (T.pack k)
      allEnv <- liftIO getEnvironment
      let backupKeys = [TEnc.encodeUtf8 (T.pack v) | (k, v) <- allEnv, "BACKUP_SIGNING_KEY_" `isPrefixOf` k]
      return (signerHMACSHA512 pKey, map signerHMACSHA512 backupKeys)
    _ -> do
      let envStr = case se of
            Development -> "development"
            Test -> "test"
          signerDir = "config/signers"
          primaryFile = signerDir </> envStr <.> "txt"

      pExists <- liftIO $ doesFileExist primaryFile
      unless pExists $ throwE $ PrimarySignerFileNotFound primaryFile
      pKey <- liftIO (T.readFile primaryFile)

      dirExists <- liftIO $ doesDirectoryExist signerDir
      allFiles <- liftIO $ if dirExists then listDirectory signerDir else return []
      let backupPrefix = envStr <> "-backup-"
          backupFiles = [signerDir </> f | f <- allFiles, backupPrefix `isPrefixOf` f, takeExtension f == ".txt"]
      sKeys <- forM (backupFiles :: [FilePath]) $ \f -> liftIO (T.readFile f)

      return (signerHMACSHA512 (TEnc.encodeUtf8 pKey), map (signerHMACSHA512 . TEnc.encodeUtf8) sKeys)

  return $ AppCfg se dbS primary secondaries

-- | Read a configuration to run the app, using the environment specified in the @NOIDED_ENV@ environment variable.
-- If that variable is missing or an unknown value, @Development@ is assumed.
readConfiguration :: IO (Either ConfigurationReadFailedError ApplicationConfig)
readConfiguration = do
  env <-
    lookupEnv "NOIDED_ENV" >>= \case
      Nothing -> return Development
      Just "Development" -> return Development
      Just "Test" -> return Test
      Just "Production" -> return Production
      Just _ -> return Development
  readConfigurationForEnv env

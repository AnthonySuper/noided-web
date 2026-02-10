{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Effect.Translate where

import Control.Monad (foldM)
import Data.Text qualified as T
import Data.Yaml (decodeFileEither)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Noided.Translate
import Noided.Web.Internal.Effect.Log
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO

-- | Monad for performing translations.
data HasTranslations :: Effect where
  GetTranslations :: HasTranslations m Translations

type instance DispatchOf HasTranslations = Dynamic

readTranslations :: (HasTranslations :> es) => Eff es Translations
readTranslations = send GetTranslations

runStaticTranslations :: Translations -> Eff (HasTranslations : es) a -> Eff es a
runStaticTranslations translations = interpret $ \_ GetTranslations -> return translations

-- | Run translations, reading them from the path as the source.
-- If the path is a /File/, the path will be read, and decoded into translations (via either YAML or JSON).
-- If the path is a /Directory/, the directory will be traversed, and all files ending in either '.yml' or '.json'
-- will be read, and the resulting translations combined together.
-- If the file has some kind of encoding error, blank translations will be used, and an error message will be logged.
--
--
-- This is useful in development mode, as this will read the file or directory *each time the effect stack is run*.
-- So when you refresh your browser, you get fresh translations!
runTranslationsFromFile :: (IOE :> es, Log :> es) => FilePath -> Eff (HasTranslations : es) a -> Eff es a
runTranslationsFromFile readPath = reinterpret (evalState @(Maybe Translations) Nothing) $ \_ GetTranslations -> do
  get >>= \case
    Just translations -> return translations
    Nothing -> do
      translations <- loadTranslations readPath
      put (Just translations)
      return translations

loadTranslations :: (IOE :> es, Log :> es) => FilePath -> Eff es Translations
loadTranslations path = do
  isDir <- liftIO $ doesDirectoryExist path
  if isDir
    then do
      files <- liftIO $ listDirectory path
      let relevantFiles = filter (\f -> let ext = takeExtension f in ext `elem` [".yml", ".yaml", ".json"]) files
      foldM (\acc f -> (acc <>) <$> loadFile (path </> f)) mempty relevantFiles
    else loadFile path

loadFile :: (IOE :> es, Log :> es) => FilePath -> Eff es Translations
loadFile path = do
  res <- liftIO $ decodeFileEither path
  case res of
    Left err -> do
      logText Error $ "Failed to decode translations from " <> T.pack path <> ": " <> T.pack (show err)
      return mempty
    Right translations -> return translations

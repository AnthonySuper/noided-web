{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Effect.Translate where

import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem.IO.ByteString
import Effectful.State.Static.Local
import Noided.Translate
import System.IO

-- | Monad for performing translations.
data HasTranslations :: Effect where
  GetTranslations :: HasTranslations m Translations

type instance DispatchOf HasTranslations = Dynamic

readTranslations :: (HasTranslations :> es) => Eff es Translations
readTranslations = send GetTranslations

runStaticTranslations :: Translations -> Eff (HasTranslations : es) a -> Eff es a
runStaticTranslations = error "TODO: implement me"

-- | Run translations, reading them from the file as the source.
-- This is useful in development mode, as this will read the file *each time the effect stack is run*.
--
-- So when you refresh your browser, you get fresh translations!
runTranslationsFromFile :: (IOE :> es) => FilePath -> Eff (HasTranslations : es) a -> Eff es a
runTranslationsFromFile readPath = reinterpret (evalState @(Maybe Translations) Nothing) $ \env GetTranslations -> do
  let readCached = get @(Maybe Translations)
  let writeCached = put @(Maybe Translations)
  error "TODO: implement this (check if cached var exists, use it if so, read and set if not)"

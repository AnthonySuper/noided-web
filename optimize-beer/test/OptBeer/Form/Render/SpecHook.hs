module OptBeer.Form.Render.SpecHook (hook) where

import Effectful (runEff)
import Noided.Translate
import Noided.Web.Effect (runIgnoringLogs)
import Noided.Web.Internal.Effect.Translate
import Test.Hspec

hook :: SpecWith Translations -> SpecWith a
hook = beforeAllWith (const readTranslationsFromConfig)

readTranslationsFromConfig :: IO Translations
readTranslationsFromConfig =
  runEff $ runIgnoringLogs $ loadTranslations "config/translations"

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Form.Render.SpecHelper where

import Control.Monad (guard)
import Data.Functor.Identity
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Lucid
import Noided.Translate
import Noided.Web.Html.Internal.Type.TranslationT
import Test.Hspec
import Text.HTML.TagSoup qualified as TagSoup

data TranslationRunner where
  RunTranslation ::
    (forall a. HtmlT (TranslationT Identity) a -> Text) ->
    TranslationRunner

withTranslationsInLocale :: Text -> SpecWith TranslationRunner -> SpecWith Translations
withTranslationsInLocale locale = beforeWith $ \trans -> do
  let msgs = translationsForLocale locale trans
      env = TranslateEnv msgs mempty
      runner :: forall a. HtmlT (TranslationT Identity) a -> Text
      runner act = LT.toStrict $ runIdentity $ getTranslationT (renderTextT act) env
  return $ RunTranslation runner

runTranslationToSoup :: TranslationRunner -> HtmlT (TranslationT Identity) a -> [TagSoup.Tag Text]
runTranslationToSoup (RunTranslation runner) act = TagSoup.parseTags (runner act)

badTranslationKeys :: [TagSoup.Attribute Text] -> [Text]
badTranslationKeys res = do
  (key, v) <- res
  guard $ "bad-key-" `T.isPrefixOf` key
  return v

-- | Get all the bad translation keys in a set.
toBadTranslationKeys :: [TagSoup.Tag Text] -> [NE.NonEmpty Text]
toBadTranslationKeys tag = do
  TagSoup.TagOpen k attrs <- tag
  guard $ k == "noided-bad-translation"
  let badTranslationValues = badTranslationKeys attrs
  Just res <- [NE.nonEmpty badTranslationValues]
  return res

formatBadSets :: NE.NonEmpty Text -> Text
formatBadSets badKeys = "(One of " <> T.intercalate ", " (NE.toList badKeys) <> ")"

-- | Assert that a rendererd soup has no `<noided-bad-translation>` tags.
assertHasNoBadTranslations :: [TagSoup.Tag Text] -> Expectation
assertHasNoBadTranslations tags =
  let badTags = toBadTranslationKeys tags
   in case badTags of
        [] -> return ()
        ts -> expectationFailure $ "Found bad translations for keys: " <> T.unpack (T.intercalate "; " (map formatBadSets ts))

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Html.Internal.Translate
  ( writeHtmlLookup,
    noidedBadTranslation_,
    renderTranslated',
    renderTranslated,
    renderErrorTranslatedWithBase',
    renderErrorTranslatedWithBase,
    renderErrorTranslated',
    renderErrorTranslated,
  )
where

import Data.Foldable
import Data.Map.Strict qualified as Map
import Data.Monoid (First (..))
import Data.Text (Text, pack)
import Lucid.Base
import Noided.Translate
import Noided.Translate.Internal.Render (renderViaWriter)
import Noided.Validation
import Noided.Web.Html.Internal.Class.FetchMessages

-- | Write a translation message as HTML.
writeHtmlLookup :: (Monad m) => Map.Map Text (HtmlT m () -> HtmlT m ()) -> Message -> TranslateParams -> HtmlT m ()
writeHtmlLookup m = renderViaWriter format toHtml
  where
    format t = Map.findWithDefault id t m

-- | Tag for the @noided-bad-translation@ custom element.
--
-- Since you're allowed to just add random elements in modern browsers, this is fine to use,
-- even if there is no custom element defined.
noidedBadTranslation_ :: (Monad m) => [Attributes] -> HtmlT m a -> HtmlT m a
noidedBadTranslation_ = term "noided-bad-translation"

toBadAttibutes :: (Foldable t, Term Text a) => t MessageKey -> [a]
toBadAttibutes foldable = fst (foldl' f ([], 1 :: Int) foldable)
  where
    f (msgs, idx) (mk :: MessageKey) =
      ( term (pack $ "bad-key-" <> show idx) (pack $ show mk) : msgs,
        idx + 1
      )

-- | Render a translation with the first matching message key.
renderTranslated :: (FetchMessages m, Foldable t) => t MessageKey -> TranslateParams -> HtmlT m ()
renderTranslated = renderTranslated' mempty

-- | Render a translation with the first matching message key, with custom formatting.
renderTranslated' :: (FetchMessages m, Foldable t) => Map.Map Text (HtmlT m () -> HtmlT m ()) -> t MessageKey -> TranslateParams -> HtmlT m ()
renderTranslated' formatMap msgKey trParams = do
  messages <- fetchMessages
  case firstMessageMatchingFoldable messages msgKey of
    Nothing ->
      noidedBadTranslation_ (toBadAttibutes msgKey) $
        maybe "NO KEY" (toHtml . show) $
          getFirst (foldMap (First . Just) msgKey)
    Just msg -> writeHtmlLookup formatMap msg trParams

-- | Render an error trnaslated with a set of base keys (and a format map)
renderErrorTranslatedWithBase' ::
  ( FetchMessages m,
    Foldable t,
    ValidationError e,
    Functor t
  ) =>
  Map.Map Text (HtmlT m () -> HtmlT m ()) ->
  t MessageKey ->
  e ->
  HtmlT m ()
renderErrorTranslatedWithBase' formatMap baseKeys e =
  renderTranslated'
    formatMap
    (fmap (addMessageKeyPart $ validationErrorKey e) baseKeys)
    (validationErrorTranslateParams e)

-- | Render an error trnaslated with a set of base keys
renderErrorTranslatedWithBase :: (FetchMessages m, Foldable t, ValidationError e, Functor t) => t MessageKey -> e -> HtmlT m ()
renderErrorTranslatedWithBase = renderErrorTranslatedWithBase' mempty

-- | Render an error translated with a format map
renderErrorTranslated' :: (FetchMessages m, ValidationError e) => Map.Map Text (HtmlT m () -> HtmlT m ()) -> e -> HtmlT m ()
renderErrorTranslated' formatMap = renderErrorTranslatedWithBase' formatMap []

-- | Render an error translated.
renderErrorTranslated :: (FetchMessages m, ValidationError e) => e -> HtmlT m ()
renderErrorTranslated = renderErrorTranslated' mempty

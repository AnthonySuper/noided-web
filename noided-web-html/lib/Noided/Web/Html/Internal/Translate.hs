{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Html.Internal.Translate
  ( writeHtmlLookup,
    noidedBadTranslation_,
    renderTranslated,
    renderErrorTranslatedWithPrefixes,
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
import Noided.Web.Html.Internal.Class.FetchHtmlFormatters
import Noided.Web.Html.Internal.Class.FetchMessages
import Noided.Web.Html.Internal.Type.HtmlFormatter

-- | Write a translation message as HTML.
writeHtmlLookup ::
  (Monad m) =>
  Map.Map Text HtmlFormatter ->
  Message ->
  TranslateParams ->
  HtmlT m ()
writeHtmlLookup m = renderViaWriter format toHtml
  where
    format t =
      maybe id useHtmlFormatter (Map.lookup t m)

-- | Tag for the @noided-bad-translation@ custom element.
--
-- Since you're allowed to just add random elements in modern browsers, this is fine to use,
-- even if there is no custom element defined.
noidedBadTranslation_ :: (Monad m) => [Attributes] -> HtmlT m a -> HtmlT m a
noidedBadTranslation_ = term "noided-bad-translation"

toBadAttributes :: (Foldable t, Term Text a) => t MessageKey -> [a]
toBadAttributes foldable = fst (foldl' f ([], 1 :: Int) foldable)
  where
    f (msgs, idx) (mk :: MessageKey) =
      ( term (pack $ "bad-key-" <> show idx) (pack $ show mk) : msgs,
        idx + 1
      )

-- | Render a translation with the first matching message key, with custom formatting.
--
-- If a message with the given key is not found, an element of type @<noided-bad-translation>@ will be rendered.
renderTranslated ::
  ( FetchMessages m,
    FetchHtmlFormatters m,
    Foldable t
  ) =>
  t MessageKey ->
  TranslateParams ->
  HtmlT m ()
renderTranslated msgKey trParams = do
  messages <- fetchMessages
  formatMap <- fetchFormatters
  case firstMessageMatchingFoldable messages msgKey of
    Nothing ->
      noidedBadTranslation_ (toBadAttributes msgKey) $
        maybe "NO KEY" (toHtml . show) $
          getFirst (foldMap (First . Just) msgKey)
    Just msg -> writeHtmlLookup formatMap msg trParams
{-# INLINE renderTranslated #-}

-- | Render an error, translated with a base path.
renderErrorTranslatedWithPrefixes ::
  ( FetchMessages m,
    FetchHtmlFormatters m,
    Foldable t,
    ValidationError e,
    Functor t
  ) =>
  t MessageKey ->
  e ->
  HtmlT m ()
renderErrorTranslatedWithPrefixes baseKeys e =
  renderTranslated
    (fmap (addMessageKeyPart $ validationErrorKey e) baseKeys)
    (validationErrorTranslateParams e)

-- | Render a single error, with its key translated.
-- The translation will be under the root @ errors @ key.
renderErrorTranslated ::
  ( FetchMessages m,
    FetchHtmlFormatters m,
    ValidationError e
  ) =>
  e ->
  HtmlT m ()
renderErrorTranslated = renderErrorTranslatedWithPrefixes ["errors"]

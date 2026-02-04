{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Html.Internal.Type.HtmlFormatter where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Lucid

-- | A formatter for rendering translations.
-- Translates a specific formatting tag into some html wrapper tag.
data HtmlFormatter where
  FormatTranslation ::
    (forall m a. (Monad m) => HtmlT m a -> HtmlT m a) ->
    HtmlFormatter

useHtmlFormatter :: (Monad m) => HtmlFormatter -> HtmlT m a -> HtmlT m a
useHtmlFormatter (FormatTranslation f) = f

type HtmlFormatters = Map Text HtmlFormatter

defaultFormatters :: HtmlFormatters
defaultFormatters =
  [ ("strong", FormatTranslation strong_),
    ("em", FormatTranslation em_),
    ("idiom", FormatTranslation i_)
  ]

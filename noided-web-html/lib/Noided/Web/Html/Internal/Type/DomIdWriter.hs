{-# LANGUAGE OverloadedStrings #-}

module Noided.Web.Html.Internal.Type.DomIdWriter
  ( DomIdWriter,
    domIdPiece,
    domIdPieceBuilder,
    domIdToBuilder,
    domIdToText,
    domIdPieceString,
  )
where

import Data.String
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)

-- | Writer to build up a DOM id.
data DomIdWriter = Written Builder | Unwritten

instance Semigroup DomIdWriter where
  Written l <> Written r = Written $ l <> "--" <> r
  Written l <> Unwritten = Written l
  Unwritten <> Written r = Written r
  Unwritten <> Unwritten = Unwritten

instance Monoid DomIdWriter where
  mempty = Unwritten

instance IsString DomIdWriter where
  fromString = Written . fromString

domIdPieceBuilder :: Builder -> DomIdWriter
domIdPieceBuilder = Written

domIdPiece :: Text -> DomIdWriter
domIdPiece = Written . fromText

domIdPieceString :: String -> DomIdWriter
domIdPieceString = Written . fromString

domIdToBuilder :: DomIdWriter -> Builder
domIdToBuilder (Written w) = w
domIdToBuilder Unwritten = mempty

domIdToText :: DomIdWriter -> Text
domIdToText =
  toStrict . toLazyText . domIdToBuilder

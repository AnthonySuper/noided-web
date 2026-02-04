{-# LANGUAGE OverloadedStrings #-}

module Noided.Web.Html.Internal.FormKey where

import Noided.Form
import Noided.Web.Html.Internal.Type.DomIdWriter

canonicalKeyToDomId :: FormCanonicalKey -> DomIdWriter
canonicalKeyToDomId (MkFormCanonicalKey k) = foldMap f k
  where
    f (CanonicalArrayPiece i) = "BRACE" <> domIdPieceString (show i) <> "ENDBRACE"
    f (CanonicalObjectPiece o) = domIdPiece o

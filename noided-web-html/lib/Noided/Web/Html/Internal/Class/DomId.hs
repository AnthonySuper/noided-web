{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Html.Internal.Class.DomId where

import Data.String (IsString (fromString))
import Data.Text (Text)
import Noided.Form
import Noided.Web.Html.Internal.FormKey
import Noided.Web.Html.Internal.Type.DomIdWriter

showingDomId :: (Show a) => a -> DomIdWriter
showingDomId = asDomId . show

-- | Class for things that can be converted to a dom id.
--
-- This is most useful for specifying how rendering should proceed.
class DomId inner where
  asDomId :: inner -> DomIdWriter

instance DomId () where
  asDomId () = "UNIT"

instance DomId DomIdWriter where
  asDomId = id

instance {-# OVERLAPPABLE #-} (DomId a) => DomId [a] where
  asDomId = foldMap asDomId

instance DomId FormCanonicalKey where
  asDomId = canonicalKeyToDomId

instance {-# OVERLAPS #-} DomId String where
  asDomId = fromString

instance DomId Text where
  asDomId = domIdPiece

instance (DomId a, DomId b) => DomId (a, b) where
  asDomId (a, b) =
    asDomId a
      <> asDomId b

instance (DomId a, DomId b, DomId c) => DomId (a, b, c) where
  asDomId (a, b, c) =
    asDomId a
      <> asDomId b
      <> asDomId c

instance DomId Int where
  asDomId = showingDomId

instance DomId Float where
  asDomId = showingDomId

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Web.Html.Internal.Class.DomId where

import Data.Int
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Word
import GHC.Generics
import Lucid (id_)
import Lucid.Base
import Noided.Form
import Noided.Web.Html.Internal.FormKey
import Noided.Web.Html.Internal.Type.DomIdWriter
import Numeric.Natural (Natural)

idFromDomId :: DomIdWriter -> Attributes
idFromDomId = id_ . domIdToText

showingDomId :: (Show a) => a -> DomIdWriter
showingDomId = asDomId . show

-- | Class for things that can be converted to a dom id.
--
-- This is most useful for specifying how rendering should proceed.
class DomId inner where
  asDomId :: inner -> DomIdWriter

instance DomId (V1 p) where
  asDomId _ = error "Void value in DomId"

instance DomId (U1 p) where
  asDomId _ = mempty

instance (DomId (f p), DomId (g p)) => DomId ((f :*: g) p) where
  asDomId (a :*: b) = asDomId a <> asDomId b

instance (DomId (f p), DomId (g p)) => DomId ((f :+: g) p) where
  asDomId (L1 a) = asDomId a
  asDomId (R1 b) = asDomId b

instance (DomId c) => DomId (K1 i c p) where
  asDomId (K1 a) = asDomId a

-- | Datatype metadata: ignore name.
instance (DomId (f p)) => DomId (M1 D c f p) where
  asDomId (M1 a) = asDomId a

-- | Selector metadata: ignore name (hybrid approach).
instance (DomId (f p)) => DomId (M1 S c f p) where
  asDomId (M1 a) = asDomId a

-- | Constructor metadata: include name (hybrid approach).
-- This ensures sum types (enums) are unique, e.g. "Active" vs "Inactive".
instance (DomId (f p), Constructor c) => DomId (M1 C c f p) where
  asDomId m@(M1 a) = fromString (conName m) <> asDomId a

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

instance (DomId a, DomId b, DomId c, DomId d) => DomId (a, b, c, d) where
  asDomId (a, b, c, d) =
    asDomId a
      <> asDomId b
      <> asDomId c
      <> asDomId d

instance (DomId a, DomId b, DomId c, DomId d, DomId e) => DomId (a, b, c, d, e) where
  asDomId (a, b, c, d, e) =
    asDomId a
      <> asDomId b
      <> asDomId c
      <> asDomId d
      <> asDomId e

instance (DomId a, DomId b, DomId c, DomId d, DomId e, DomId f) => DomId (a, b, c, d, e, f) where
  asDomId (a, b, c, d, e, f) =
    asDomId a
      <> asDomId b
      <> asDomId c
      <> asDomId d
      <> asDomId e
      <> asDomId f

instance (DomId a, DomId b, DomId c, DomId d, DomId e, DomId f, DomId g) => DomId (a, b, c, d, e, f, g) where
  asDomId (a, b, c, d, e, f, g) =
    asDomId a
      <> asDomId b
      <> asDomId c
      <> asDomId d
      <> asDomId e
      <> asDomId f
      <> asDomId g

instance (DomId a, DomId b, DomId c, DomId d, DomId e, DomId f, DomId g, DomId h) => DomId (a, b, c, d, e, f, g, h) where
  asDomId (a, b, c, d, e, f, g, h) =
    asDomId a
      <> asDomId b
      <> asDomId c
      <> asDomId d
      <> asDomId e
      <> asDomId f
      <> asDomId g
      <> asDomId h

instance (DomId a, DomId b, DomId c, DomId d, DomId e, DomId f, DomId g, DomId h, DomId i) => DomId (a, b, c, d, e, f, g, h, i) where
  asDomId (a, b, c, d, e, f, g, h, i) =
    asDomId a
      <> asDomId b
      <> asDomId c
      <> asDomId d
      <> asDomId e
      <> asDomId f
      <> asDomId g
      <> asDomId h
      <> asDomId i

instance (DomId a, DomId b, DomId c, DomId d, DomId e, DomId f, DomId g, DomId h, DomId i, DomId j) => DomId (a, b, c, d, e, f, g, h, i, j) where
  asDomId (a, b, c, d, e, f, g, h, i, j) =
    asDomId a
      <> asDomId b
      <> asDomId c
      <> asDomId d
      <> asDomId e
      <> asDomId f
      <> asDomId g
      <> asDomId h
      <> asDomId i
      <> asDomId j

instance DomId Bool where
  asDomId True = "true"
  asDomId False = "false"

instance DomId Int where
  asDomId = showingDomId

instance DomId Int8 where
  asDomId = showingDomId

instance DomId Int16 where
  asDomId = showingDomId

instance DomId Int32 where
  asDomId = showingDomId

instance DomId Int64 where
  asDomId = showingDomId

instance DomId Integer where
  asDomId = showingDomId

instance DomId Word where
  asDomId = showingDomId

instance DomId Word8 where
  asDomId = showingDomId

instance DomId Word16 where
  asDomId = showingDomId

instance DomId Word32 where
  asDomId = showingDomId

instance DomId Word64 where
  asDomId = showingDomId

instance DomId Natural where
  asDomId = showingDomId

instance DomId Float where
  asDomId = showingDomId

instance DomId Double where
  asDomId = showingDomId

instance (DomId a) => DomId (Maybe a) where
  asDomId Nothing = "Nothing"
  asDomId (Just a) = "Just" <> asDomId a

instance (Generic a, DomId (Rep a ())) => DomId (Generically a) where
  asDomId (Generically a) = asDomId (from a :: Rep a ())

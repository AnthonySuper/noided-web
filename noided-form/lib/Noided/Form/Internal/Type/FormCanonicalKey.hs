module Noided.Form.Internal.Type.FormCanonicalKey where

import Data.Foldable
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder qualified as LB
import GHC.Generics

-- | A piece of a form canonical key.
data FormCanonicalPiece = CanonicalObjectPiece Text | CanonicalArrayPiece Int
  deriving (Show, Read, Eq, Ord, Generic)

-- | Canonical key of a form piece, which is just a list of 'FormCanonicalPiece' parts.
newtype FormCanonicalKey = MkFormCanonicalKey {getFormCanonicalKey :: Seq.Seq FormCanonicalPiece}
  deriving (Show, Read, Eq, Ord, Generic)

emptyCanonicalKey :: FormCanonicalKey
emptyCanonicalKey = MkFormCanonicalKey mempty

appendCanonicalPiece :: FormCanonicalKey -> FormCanonicalPiece -> FormCanonicalKey
appendCanonicalPiece (MkFormCanonicalKey k) p = MkFormCanonicalKey (k Seq.:|> p)

canonicalKeyToFieldName :: FormCanonicalKey -> Text
canonicalKeyToFieldName =
  toStrict . LB.toLazyText . canonicalKeyToFieldNameBuilder

canonicalKeyToFieldNameBuilder :: FormCanonicalKey -> LB.Builder
canonicalKeyToFieldNameBuilder (MkFormCanonicalKey br) = fst $ foldl' f (mempty, False) br
  where
    f !(buff, written) (CanonicalObjectPiece o)
      | written = (buff <> LB.fromString "[" <> LB.fromText o <> LB.fromString "]", written)
      | otherwise = (buff <> LB.fromText o, True)
    f !(buff, _) (CanonicalArrayPiece _) = (buff <> LB.fromString "[]", True)

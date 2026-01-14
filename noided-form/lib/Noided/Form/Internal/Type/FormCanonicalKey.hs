module Noided.Form.Internal.Type.FormCanonicalKey where

import Data.Sequence qualified as Seq
import Data.Text (Text)
import GHC.Generics

-- | A piece of a form canonical key.
data FormCanonicalPiece = CanonicalObjectPiece Text | CanonicalArrayPiece Int
  deriving (Show, Read, Eq, Ord, Generic)

-- | Canonical key of a form piece, which is just a list of 'FormCanonicalPiece' parts.
newtype FormCanonicalKey = MkFormCanonicalKey {getFormCanonicalKey :: Seq.Seq FormCanonicalPiece}
  deriving (Show, Read, Eq, Ord, Generic)

appendCanonicalPiece :: FormCanonicalKey -> FormCanonicalPiece -> FormCanonicalKey
appendCanonicalPiece (MkFormCanonicalKey k) p = MkFormCanonicalKey (k Seq.:|> p)

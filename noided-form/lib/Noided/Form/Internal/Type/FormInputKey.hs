module Noided.Form.Internal.Type.FormInputKey where

import Control.DeepSeq (NFData)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import GHC.Generics

-- | Input pieces of a form.
data FormInputPiece
  = -- | A text piece.
    TextPiece !Text
  | -- | An empty braces piece (like @ [] @).
    BracesPiece
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData FormInputPiece

type FormInputKey = Seq.Seq FormInputPiece

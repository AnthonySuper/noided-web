module Noided.Sql.Internal.Type.WindowFrame where

import Data.Kind
import GHC.Generics

data FrameMode = Rows | Groups | Range
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

-- | Singleton type for 'FrameMode'.
data FrameModeSing (fr :: FrameMode) where
  RowsSing :: FrameModeSing Rows
  GroupsSing :: FrameModeSing Groups
  RangeSing :: FrameModeSing Range

data FrameOrdering = AnyOrdering | SingleOrdering Type
  deriving (Generic)

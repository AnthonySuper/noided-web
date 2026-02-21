module OptBeer.ValidationError.ValueTaken where

import GHC.Generics
import Noided.Validation

data ValueTaken = ValueTaken
  deriving (Show, Eq, Ord, Generic)

instance ValidationError ValueTaken

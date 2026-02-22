module OptBeer.ValidationError.InvalidCredentials where

import GHC.Generics
import Noided.Validation

data InvalidCredentials = InvalidCredentials
  deriving (Show, Eq, Ord, Generic)

instance ValidationError InvalidCredentials

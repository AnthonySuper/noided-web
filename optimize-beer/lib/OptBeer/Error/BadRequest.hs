module OptBeer.Error.BadRequest where

import Data.Text
import GHC.Generics
import Noided.Validation (ValidationError)

-- | Bad request error: a request was not valid.
newtype BadRequest = BadRequest {reqMsg :: Text}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

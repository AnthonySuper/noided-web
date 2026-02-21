module OptBeer.ValidationError.DoesNotMatchConfirmation where

import GHC.Generics
import Noided.Validation

data DoesNotMatchConfirmation = DoesNotMatchConfirmation
  deriving (Show, Eq, Ord, Generic)

instance ValidationError DoesNotMatchConfirmation

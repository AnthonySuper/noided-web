module Noided.Validation.Internal.ValidationError.FailedValidation where

import GHC.Generics
import Noided.Validation.Internal.ValidationError

-- | The absolute most basic validation error possible.
-- Tells you that validation failed, but not why.
data FailedValidation = FailedValidation
  deriving (Show, Read, Eq, Ord, Generic)

-- | All errors can be converted to this type, because it is the most basic.
instance ValidationError FailedValidation where
  fromSomeValidationError _ = Just FailedValidation
  validationErrorTranslateParams _ = mempty

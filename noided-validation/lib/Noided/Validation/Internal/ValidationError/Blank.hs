module Noided.Validation.Internal.ValidationError.Blank where

import GHC.Generics
import Noided.Validation.Internal.ValidationError

-- | Validation error that occurrs when something was
-- blank unexpectedly.
data Blank = Blank
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance ValidationError Blank where
  validationErrorTranslateParams _ = mempty

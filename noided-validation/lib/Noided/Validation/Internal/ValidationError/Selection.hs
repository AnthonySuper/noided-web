{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Noided.Validation.Internal.ValidationError.Selection where

import GHC.Generics
import Noided.Validation.Internal.ValidationError

-- | Error when a value is not one of the allowed options.
data InvalidSelection = InvalidSelection
  deriving stock (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance ValidationError InvalidSelection where
  validationErrorTranslateParams InvalidSelection = mempty

-- | Error when a value is one of the forbidden options.
data ForbiddenSelection = ForbiddenSelection
  deriving stock (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance ValidationError ForbiddenSelection where
  validationErrorTranslateParams ForbiddenSelection = mempty

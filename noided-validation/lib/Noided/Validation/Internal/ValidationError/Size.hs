{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Noided.Validation.Internal.ValidationError.Size where

import GHC.Generics
import Noided.Validation.Internal.ValidationError

newtype TooLarge a = TooLarge
  {limit :: a}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

newtype TooSmall a = TooSmall
  {limit :: a}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

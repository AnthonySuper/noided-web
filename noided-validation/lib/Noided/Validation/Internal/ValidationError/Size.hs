{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Noided.Validation.Internal.ValidationError.Size where

import GHC.Generics
import Noided.Validation.Internal.ValidationError

data TooLarge = TooLarge
  { limit :: Integer,
    actual :: Integer
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

data TooSmall = TooSmall
  { limit :: Integer,
    actual :: Integer
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

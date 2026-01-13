{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Noided.Validation.Internal.ValidationError.Number where

import GHC.Generics
import Noided.Validation.Internal.ValidationError

newtype NotOdd = NotOdd
  { actualValue :: Integer
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

newtype NotEven = NotEven
  { actualValue :: Integer
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

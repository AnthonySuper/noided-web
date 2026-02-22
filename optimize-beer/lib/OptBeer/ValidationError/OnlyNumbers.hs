{-# LANGUAGE DeriveAnyClass #-}

module OptBeer.ValidationError.OnlyNumbers where

import GHC.Generics (Generic)
import Noided.Validation (ValidationError)

-- | Error when a value that should be a name contains only numbers.
data OnlyNumbers = OnlyNumbers
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

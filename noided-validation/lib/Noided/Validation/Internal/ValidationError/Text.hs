{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Noided.Validation.Internal.ValidationError.Text where

import Data.Text (Text)
import GHC.Generics
import Noided.Validation.Internal.ValidationError

data DoesNotStartWith = DoesNotStartWith
  { expectedPrefix :: Text,
    actualValue :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

data DoesNotEndWith = DoesNotEndWith
  { expectedSuffix :: Text,
    actualValue :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

data DoesNotContain = DoesNotContain
  { expectedSubstring :: Text,
    actualValue :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

data Contains = Contains
  { forbiddenSubstring :: Text,
    actualValue :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

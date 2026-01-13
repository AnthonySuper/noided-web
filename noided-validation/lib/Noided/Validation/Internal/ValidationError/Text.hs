{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Noided.Validation.Internal.ValidationError.Text where

import Data.Text (Text)
import GHC.Generics
import Noided.Validation.Internal.ValidationError

newtype DoesNotStartWith = DoesNotStartWith
  { expectedPrefix :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

newtype DoesNotEndWith = DoesNotEndWith
  {expectedSuffix :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

newtype DoesNotContain = DoesNotContain
  {expectedSubstring :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

newtype Contains = Contains
  {forbiddenSubstring :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

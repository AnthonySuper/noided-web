{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module OptBeer.ValidationError.PasswordComplexity where

import Data.Text (Text)
import GHC.Generics
import Noided.Validation

data PasswordTooShort = PasswordTooShort { minLength :: Int, providedLength :: Int }
  deriving (Show, Eq, Ord, Generic, ValidationError)

data PasswordTooLong = PasswordTooLong { maxLength :: Int, providedLength :: Int }
  deriving (Show, Eq, Ord, Generic, ValidationError)

data NotEnoughReqChars = NotEnoughReqChars { category :: Text, minAmount :: Int, providedAmount :: Int }
  deriving (Show, Eq, Ord, Generic, ValidationError)

data InvalidCharacters = InvalidCharacters { chars :: Text }
  deriving (Show, Eq, Ord, Generic, ValidationError)

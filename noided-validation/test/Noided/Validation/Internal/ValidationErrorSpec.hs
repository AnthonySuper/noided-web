{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Validation.Internal.ValidationErrorSpec where

import Data.Text (Text)
import GHC.Generics (Generic)
import Noided.Validation.Internal.ValidationError
import Test.Hspec

-- Define some sample validation errors
newtype ErrorA = ErrorA {field :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

newtype ErrorB = ErrorB {code :: Integer}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

spec :: Spec
spec = do
  describe "SomeValidationError" $ do
    it "implements Show correctly" $ do
      let err = ErrorA "value"
      show (toSomeValidationError err) `shouldBe` show err

    it "implements Eq correctly" $ do
      let e1 = toSomeValidationError (ErrorA "a")
      let e2 = toSomeValidationError (ErrorA "a")
      let e3 = toSomeValidationError (ErrorA "b")
      let e4 = toSomeValidationError (ErrorB 1)

      e1 `shouldBe` e2
      e1 `shouldNotBe` e3
      e1 `shouldNotBe` e4

    it "implements Ord correctly" $ do
      let e1 = toSomeValidationError (ErrorA "a")
      let e2 = toSomeValidationError (ErrorA "b")

      e1 < e2 `shouldBe` True
      e2 > e1 `shouldBe` True

      -- Different types should be comparable
      let eb = toSomeValidationError (ErrorB 1)
      (e1 < eb || e1 > eb) `shouldBe` True

  describe "validationErrorKey" $ do
    it "uses the type constructor name by default" $ do
      let err = ErrorA "test"
      validationErrorKey err `shouldBe` "ErrorA"

      let someErr = toSomeValidationError err
      validationErrorKey someErr `shouldBe` "ErrorA"

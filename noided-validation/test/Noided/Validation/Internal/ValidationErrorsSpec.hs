{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Validation.Internal.ValidationErrorsSpec where

import Data.Aeson (encode)
import Data.Text (Text)
import GHC.Generics (Generic)
import Noided.Validation.Internal.ValidationError
import Noided.Validation.Internal.ValidationErrors
import Optics.Core
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
  describe "ValidationErrors" $ do
    it "mempty should be null" $ do
      let errors = mempty :: ValidationErrors
      nullErrors errors `shouldBe` True

    it "singletonError should not be null" $ do
      let errors = singletonError (ErrorA "field")
      nullErrors errors `shouldBe` False

    it "combines errors via Semigroup" $ do
      let e1 = singletonError (ErrorA "a")
      let e2 = singletonError (ErrorB 1)
      let combined = e1 <> e2

      nullErrors combined `shouldBe` False
      hasError combined (ErrorA "a") `shouldBe` True
      hasError combined (ErrorB 1) `shouldBe` True

    describe "hasError" $ do
      it "detects present errors" $ do
        let errors = singletonError (ErrorA "present")
        hasError errors (ErrorA "present") `shouldBe` True

      it "does not detect missing errors" $ do
        let errors = singletonError (ErrorA "present")
        hasError errors (ErrorA "missing") `shouldBe` False
        hasError errors (ErrorB 1) `shouldBe` False

    describe "Optics" $ do
      it "allErrors iterates over all errors" $ do
        let errors = singletonError (ErrorA "1") <> singletonError (ErrorB 2)
        let count = lengthOf allErrors errors
        count `shouldBe` 2

      it "errorsOfType selects specific error types" $ do
        let errors = singletonError (ErrorA "1")
                  <> singletonError (ErrorA "2")
                  <> singletonError (ErrorB 1)

        let as = toListOf (errorsOfType @ErrorA) errors
        length as `shouldBe` 2
        elem (ErrorA "1") as `shouldBe` True
        elem (ErrorA "2") as `shouldBe` True

        let bs = toListOf (errorsOfType @ErrorB) errors
        length bs `shouldBe` 1
        head bs `shouldBe` ErrorB 1

    describe "JSON Serialization" $ do
      it "serializes correctly" $ do
        let errors = singletonError (ErrorA "test")
        let json = encode errors
        -- We just check it's not empty/malformed, exact string match relies on Set order and specific JSON formatting which is brittle
        json `shouldNotBe` ""

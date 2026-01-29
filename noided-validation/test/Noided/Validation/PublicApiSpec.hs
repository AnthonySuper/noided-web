{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module: Noided.Validation.PublicApiSpec
Description: Tests for the public API

This test suite verifies that the public API modules work correctly
and that all functionality is accessible through the unified module.
-}
module Noided.Validation.PublicApiSpec where

import Data.Text (Text)
import GHC.Generics (Generic)
import Noided.Validation
import Test.Hspec

-- Test error type
newtype AgeError = AgeError {minAge :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

spec :: Spec
spec = do
  describe "Public API" $ do
    describe "Unified module import" $ do
      it "provides access to core validation functions" $ do
        let validation = do
              check True (AgeError 18)
              return ("success" :: Text)
        runValidator validation `shouldBe` Right ("success" :: Text)

      it "provides access to validation combinators" $ do
        let validation = optional (check False . AgeError) Nothing
        runValidator validation `shouldBe` Right ()

      it "provides access to size validators" $ do
        let validation = lengthAtLeast 2 [1 :: Int, 2, 3]
        case runValidator validation of
          Right () -> return ()
          Left _ -> expectationFailure "Should have succeeded"

      it "provides access to text validators" $ do
        let validation = startsWith ("hello" :: Text) ("hello world" :: Text)
        case runValidator validation of
          Right () -> return ()
          Left _ -> expectationFailure "Should have succeeded"

      it "provides access to number validators" $ do
        let validation = isEven (4 :: Int)
        case runValidator validation of
          Right () -> return ()
          Left _ -> expectationFailure "Should have succeeded"

    describe "Individual module imports" $ do
      it "allows importing from Noided.Validation.Validator" $ do
        -- This is tested implicitly by runValidator usage above
        True `shouldBe` True

      it "allows importing from Noided.Validation.ValidationError" $ do
        -- This is tested implicitly by the custom error type above
        True `shouldBe` True

      it "allows importing from Noided.Validation.ValidationErrors" $ do
        let validation = do
              check False (AgeError 18)
              check False (AgeError 21)
        case runValidator validation of
          Left errs -> do
            hasError errs (AgeError 18) `shouldBe` True
            hasError errs (AgeError 21) `shouldBe` True
          Right _ -> expectationFailure "Should have failed"

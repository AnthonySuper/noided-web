{-# LANGUAGE OverloadedStrings #-}

module Noided.Validation.Internal.Validate.TextSpec where

import Data.Either (isLeft, isRight)
import Noided.Validation.Internal.Validate.Text
import Noided.Validation.Internal.ValidationError.Text
import Noided.Validation.Internal.ValidationErrors (hasError)
import Noided.Validation.Internal.Validator
import Test.Hspec

spec :: Spec
spec = do
  describe "Text Validations" $ do
    describe "startsWith" $ do
      it "succeeds when text starts with prefix" $ do
        runValidator (startsWith "hello" "hello world") `shouldSatisfy` isRight

      it "fails when text does not start with prefix" $ do
        let result = runValidator (startsWith "bye" "hello world")
        result `shouldSatisfy` isLeft
        case result of
          Left errs -> hasError errs (DoesNotStartWith "bye") `shouldBe` True
          Right _ -> expectationFailure "Should have failed"

    describe "endsWith" $ do
      it "succeeds when text ends with suffix" $ do
        runValidator (endsWith "world" "hello world") `shouldSatisfy` isRight

      it "fails when text does not end with suffix" $ do
        let result = runValidator (endsWith "earth" "hello world")
        result `shouldSatisfy` isLeft
        case result of
          Left errs -> hasError errs (DoesNotEndWith "earth") `shouldBe` True
          Right _ -> expectationFailure "Should have failed"

    describe "contains" $ do
      it "succeeds when text contains substring" $ do
        runValidator (contains "lo w" "hello world") `shouldSatisfy` isRight

      it "fails when text does not contain substring" $ do
        let result = runValidator (contains "z" "hello world")
        result `shouldSatisfy` isLeft
        case result of
          Left errs -> hasError errs (DoesNotContain "z") `shouldBe` True
          Right _ -> expectationFailure "Should have failed"

    describe "notContains" $ do
      it "succeeds when text does not contain substring" $ do
        runValidator (notContains "z" "hello world") `shouldSatisfy` isRight

      it "fails when text contains substring" $ do
        let result = runValidator (notContains "o" "hello world")
        result `shouldSatisfy` isLeft
        case result of
          Left errs -> hasError errs (Contains "o") `shouldBe` True
          Right _ -> expectationFailure "Should have failed"

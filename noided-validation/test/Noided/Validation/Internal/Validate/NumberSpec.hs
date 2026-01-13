{-# LANGUAGE OverloadedStrings #-}

module Noided.Validation.Internal.Validate.NumberSpec where

import Data.Either (isLeft, isRight)
import Noided.Validation.Internal.Validate.Number
import Noided.Validation.Internal.ValidationError.Number
import Noided.Validation.Internal.Validator
import Noided.Validation.Internal.ValidationErrors (hasError)
import Test.Hspec

spec :: Spec
spec = do
  describe "Number Validations" $ do
    describe "isOdd" $ do
      it "succeeds for odd numbers" $ do
        runValidator (isOdd (3 :: Int)) `shouldSatisfy` isRight

      it "fails for even numbers" $ do
        let result = runValidator (isOdd (2 :: Int))
        result `shouldSatisfy` isLeft
        case result of
            Left errs -> hasError errs (NotOdd 2) `shouldBe` True
            Right _ -> expectationFailure "Should have failed"

    describe "isEven" $ do
      it "succeeds for even numbers" $ do
        runValidator (isEven (2 :: Int)) `shouldSatisfy` isRight

      it "fails for odd numbers" $ do
        let result = runValidator (isEven (3 :: Int))
        result `shouldSatisfy` isLeft
        case result of
            Left errs -> hasError errs (NotEven 3) `shouldBe` True
            Right _ -> expectationFailure "Should have failed"

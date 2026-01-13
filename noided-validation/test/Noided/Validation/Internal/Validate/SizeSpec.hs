{-# LANGUAGE OverloadedStrings #-}

module Noided.Validation.Internal.Validate.SizeSpec where

import Data.Either (isLeft)
import Noided.Validation.Internal.Validate.Size
import Noided.Validation.Internal.ValidationError.Size
import Noided.Validation.Internal.ValidationErrors
import Noided.Validation.Internal.Validator
import Test.Hspec

spec :: Spec
spec = do
  describe "Length Validators" $ do
    describe "lengthAtLeast" $ do
      it "succeeds when length is equal to min" $ do
        runValidator (lengthAtLeast 3 [1, 2, 3 :: Int]) `shouldBe` Right ()

      it "succeeds when length is greater than min" $ do
        runValidator (lengthAtLeast 3 [1, 2, 3, 4 :: Int]) `shouldBe` Right ()

      it "fails when length is less than min" $ do
        let res = runValidator (lengthAtLeast 3 [1, 2 :: Int])
        res `shouldSatisfy` isLeft
        case res of
          Left errs -> hasError errs (TooSmall 3 2) `shouldBe` True
          Right _ -> expectationFailure "Should have failed"

    describe "lengthAtMost" $ do
      it "succeeds when length is equal to max" $ do
        runValidator (lengthAtMost 3 [1, 2, 3 :: Int]) `shouldBe` Right ()

      it "fails when length is greater than max" $ do
        let res = runValidator (lengthAtMost 2 [1, 2, 3 :: Int])
        res `shouldSatisfy` isLeft
        case res of
          Left errs -> hasError errs (TooLarge 2 3) `shouldBe` True
          Right _ -> expectationFailure "Should have failed"

    describe "lengthBetween" $ do
      it "succeeds within range" $ do
        runValidator (lengthBetween 2 4 [1, 2, 3 :: Int]) `shouldBe` Right ()

      it "fails when too small" $ do
        let res = runValidator (lengthBetween 3 5 [1, 2 :: Int])
        case res of
          Left errs -> hasError errs (TooSmall 3 2) `shouldBe` True
          Right _ -> expectationFailure "Should have failed"

      it "fails when too large" $ do
        let res = runValidator (lengthBetween 1 2 [1, 2, 3 :: Int])
        case res of
          Left errs -> hasError errs (TooLarge 2 3) `shouldBe` True
          Right _ -> expectationFailure "Should have failed"

  describe "Numeric Validators" $ do
    describe "valueAtLeast" $ do
      it "succeeds when value is sufficient" $ do
        runValidator (valueAtLeast 10 (10 :: Int)) `shouldBe` Right ()
        runValidator (valueAtLeast 10 (15 :: Int)) `shouldBe` Right ()

      it "fails when value is too small" $ do
        let res = runValidator (valueAtLeast 10 (5 :: Int))
        case res of
          Left errs -> hasError errs (TooSmall 10 5) `shouldBe` True
          Right _ -> expectationFailure "Should have failed"

    describe "valueAtMost" $ do
      it "succeeds when value is within limit" $ do
        runValidator (valueAtMost 10 (10 :: Int)) `shouldBe` Right ()
        runValidator (valueAtMost 10 (5 :: Int)) `shouldBe` Right ()

      it "fails when value is too large" $ do
        let res = runValidator (valueAtMost 10 (15 :: Int))
        case res of
          Left errs -> hasError errs (TooLarge 10 15) `shouldBe` True
          Right _ -> expectationFailure "Should have failed"

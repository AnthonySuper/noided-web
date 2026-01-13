{-# LANGUAGE OverloadedStrings #-}

module Noided.Validation.Internal.Validate.CombinatorsSpec where

import Noided.Validation.Internal.Validate.Combinators
import Noided.Validation.Internal.Validate.Blank
import Noided.Validation.Internal.ValidationError.Blank
import Noided.Validation.Internal.ValidationErrors
import Noided.Validation.Internal.Validator
import Test.Hspec

spec :: Spec
spec = do
  describe "Combinators" $ do
    describe "optional" $ do
      it "succeeds when value is Nothing" $ do
        runValidator (optional foldableNonBlank (Nothing :: Maybe [Int])) `shouldBe` Right ()

      it "runs validator when value is Just" $ do
        runValidator (optional foldableNonBlank (Just ([1] :: [Int]))) `shouldBe` Right ()

        let res = runValidator (optional foldableNonBlank (Just ([] :: [Int])))
        case res of
          Left errs -> hasError errs Blank `shouldBe` True
          Right _ -> expectationFailure "Should have failed"

    describe "validateIf" $ do
      it "runs validator when condition is True" $ do
        let res = runValidator (validateIf True (foldableNonBlank ([] :: [Int])))
        case res of
          Left errs -> hasError errs Blank `shouldBe` True
          Right _ -> expectationFailure "Should have failed"

      it "skips validator when condition is False" $ do
        runValidator (validateIf False (foldableNonBlank ([] :: [Int]))) `shouldBe` Right ()

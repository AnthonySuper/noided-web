{-# LANGUAGE OverloadedStrings #-}

module Noided.Validation.Internal.Validate.SelectionSpec where

import Data.Text (Text)
import Noided.Validation.Internal.Validate.Selection
import Noided.Validation.Internal.ValidationError.Selection
import Noided.Validation.Internal.ValidationErrors
import Noided.Validation.Internal.Validator
import Test.Hspec

spec :: Spec
spec = do
  describe "Selection Validators" $ do
    describe "oneOf" $ do
      it "succeeds when value is in options" $ do
        runValidator (oneOf ["a", "b"] ("a" :: Text)) `shouldBe` Right ()

      it "fails when value is not in options" $ do
        let res = runValidator (oneOf ["a", "b"] ("c" :: Text))
        case res of
          Left errs -> hasError errs (InvalidSelection ["a", "b"] ("c" :: Text)) `shouldBe` True
          Right _ -> expectationFailure "Should have failed"

    describe "noneOf" $ do
      it "succeeds when value is not in forbidden" $ do
        runValidator (noneOf ["bad"] ("good" :: Text)) `shouldBe` Right ()

      it "fails when value is in forbidden" $ do
        let res = runValidator (noneOf ["bad", "evil"] ("bad" :: Text))
        case res of
          Left errs -> hasError errs (ForbiddenSelection ["bad", "evil"] ("bad" :: Text)) `shouldBe` True
          Right _ -> expectationFailure "Should have failed"

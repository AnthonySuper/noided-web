{-# LANGUAGE OverloadedStrings #-}

module Noided.Validation.Internal.Validate.BlankSpec where

import Data.Either (isLeft)
import Data.List.NonEmpty qualified as NE
import Noided.Validation.Internal.Validate.Blank
import Noided.Validation.Internal.ValidationError.Blank
import Noided.Validation.Internal.ValidationErrors
import Noided.Validation.Internal.Validator
import Test.Hspec

spec :: Spec
spec = do
  describe "listNonBlankNE" $ do
    it "returns NonEmpty list when input is not empty" $ do
      let result = runValidator (listNonBlankNE [1, 2, 3 :: Int])
      result `shouldBe` Right (NE.fromList [1, 2, 3])

    it "fails fatally with Blank when input is empty" $ do
      let result = runValidator (listNonBlankNE ([] :: [Int]))
      result `shouldSatisfy` isLeft
      case result of
        Left errs -> hasError errs Blank `shouldBe` True
        Right _ -> expectationFailure "Should have failed"

  describe "foldableNonBlank" $ do
    it "succeeds when foldable is not empty" $ do
      let result = runValidator (foldableNonBlank (Just ("value" :: String)))
      result `shouldBe` Right ()

    it "fails non-fatally with Blank when foldable is empty" $ do
      let result = runValidator (foldableNonBlank (Nothing :: Maybe String))
      result `shouldSatisfy` isLeft
      case result of
        Left errs -> hasError errs Blank `shouldBe` True
        Right _ -> expectationFailure "Should have failed"
      
    it "accumulates with other errors (non-fatal check)" $ do
      let validation = do
            foldableNonBlank ([] :: [Int])
            foldableNonBlank (Nothing :: Maybe Int)
      
      let result = runValidator validation
      case result of
        Left errs -> do
          -- Since it's the same error type (Blank), it acts like a Set, 
          -- so we really just check Blank is present.
          -- If we wanted to count them, we'd need a different data structure than Set SomeValidationError
          hasError errs Blank `shouldBe` True
        Right _ -> expectationFailure "Should have failed"

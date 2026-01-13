{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Validation.Internal.ValidatorSpec where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Data.Either (isLeft)
import Data.Text (Text)
import GHC.Generics (Generic)
import Noided.Validation.Internal.ValidationError
import Noided.Validation.Internal.ValidationErrors
import Noided.Validation.Internal.Validator
import Test.Hspec

-- Reuse sample errors
newtype ErrorA = ErrorA {field :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

newtype ErrorB = ErrorB {code :: Integer}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

spec :: Spec
spec = do
  describe "Validator" $ do
    describe "Non-fatal errors" $ do
      it "collects multiple non-fatal errors" $ do
        let validation = do
              check False (ErrorA "1")
              check False (ErrorA "2")
              return ()

        let result = runValidator validation
        result `shouldSatisfy` isLeft
        case result of
          Left errs -> do
            hasError errs (ErrorA "1") `shouldBe` True
            hasError errs (ErrorA "2") `shouldBe` True
          Right _ -> expectationFailure "Should have failed"

      it "succeeds if checks pass" $ do
        let validation = do
              check True (ErrorA "1")
              return ("success" :: String)

        runValidator validation `shouldBe` Right "success"

    describe "Fatal errors" $ do
      it "stops immediately on fatal error" $ do
        let validation = do
              require False (ErrorA "fatal")
              check False (ErrorB 1) -- Should not be reached
              return ()

        let result = runValidator validation
        case result of
          Left errs -> do
            hasError errs (ErrorA "fatal") `shouldBe` True
            hasError errs (ErrorB 1) `shouldBe` False
          Right _ -> expectationFailure "Should have failed"

      it "combines prior non-fatal errors with the fatal error" $ do
        let validation = do
              check False (ErrorA "non-fatal")
              require False (ErrorB 99)
              return ()

        let result = runValidator validation
        case result of
          Left errs -> do
            hasError errs (ErrorA "non-fatal") `shouldBe` True
            hasError errs (ErrorB 99) `shouldBe` True
          Right _ -> expectationFailure "Should have failed"

    describe "Alternative instance" $ do
      it "uses second branch if first fails fatally" $ do
        let validation = do
              failFatal (ErrorA "1") <|> failFatal (ErrorB 2)

        -- ExceptT's Alternative typically tries the second if the first throws an error
        -- Let's verify our specific behavior
        let result = runValidator validation
        case result of
           Left errs -> hasError errs (ErrorB 2) `shouldBe` True
           Right _ -> expectationFailure "Should have failed"

      it "returns first success" $ do
        let validation = do
              pure ("success" :: String) <|> failFatal (ErrorB 2)
        runValidator validation `shouldBe` Right "success"

    describe "MonadTrans" $ do
      it "lifts actions from the base monad" $ do
        result <- runValidatorT $ do
          _ <- lift (return ("effect" :: String))
          check True (ErrorA "fail")
          return ("done" :: String)
        result `shouldBe` Right "done"

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Row.Internal.Type.WrappedRowSpec (spec) where

import Data.Aeson
import Data.Functor.Identity
import Noided.Row.Internal.Type.RowElement
import Noided.Row.Internal.Type.RowLabel
import Noided.Row.Internal.Type.WrappedRow
import Optics.Core
import Test.Hspec
import Text.Read

emptyIdentRow :: WrappedRow '[] Identity
emptyIdentRow = EmptyWrappedRow

elementIdentRow :: WrappedRow '["age" :=> Int] Identity
elementIdentRow =
  #age :==> Identity 10 :::%? EmptyWrappedRow

largerIdentRow :: WrappedRow ["name" :=> String, "age" :=> Int] Identity
largerIdentRow = #name :==> Identity "bob" :::%? elementIdentRow

readShowSpec :: Spec
readShowSpec = describe "reading/showing" $ do
  describe "empty rows" $ do
    it "can show" $
      show emptyIdentRow `shouldBe` "EmptyWrappedRow"
    it "can read" $
      readEither "EmptyWrappedRow" `shouldBe` (Right emptyIdentRow)
    it "can round-trip" $ do
      readEither (show emptyIdentRow) `shouldBe` (Right emptyIdentRow)
  describe "rows with a single item" $ do
    it "can show" $
      show elementIdentRow
        `shouldBe` "#age :==> Identity 10 :::%? EmptyWrappedRow"
    it "can read" $ do
      readEither "#age :==> Identity 10 :::%? EmptyWrappedRow"
        `shouldBe` Right elementIdentRow
  describe "rows with two items" $ do
    it "can round-trip" $ do
      readEither (show largerIdentRow) `shouldBe` Right largerIdentRow

lensSpec :: Spec
lensSpec = describe "optics lenses" $ do
  it "can read a head item" $ do
    largerIdentRow ^. #name `shouldBe` Identity "bob"
  it "can set a larger item" $ do
    (largerIdentRow & #age .~ Identity (10.0 :: Float))
      `shouldBe` (#name :==> Identity "bob" :::%? #age :==> Identity 10.0 :::%? EmptyWrappedRow)

eqSpec :: Spec
eqSpec = describe "equality" $ do
  it "works with empty rows" $ emptyIdentRow `shouldBe` emptyIdentRow
  it "works with single-element rows" $
    elementIdentRow `shouldBe` elementIdentRow
  it "works with big rows" $
    largerIdentRow `shouldBe` largerIdentRow

jsonSpec :: Spec
jsonSpec = describe "to/from JSON" $ do
  it "works with empty rows" $ jsonShouldRoundtrip emptyIdentRow
  it "works with a single element" $ jsonShouldRoundtrip elementIdentRow
  it "works with a larger row" $ jsonShouldRoundtrip largerIdentRow

jsonShouldRoundtrip :: (Show a, Eq a, FromJSON a, ToJSON a) => a -> Expectation
jsonShouldRoundtrip a =
  decode (encode a) `shouldBe` Just a

spec :: Spec
spec = do
  readShowSpec
  lensSpec
  eqSpec
  jsonSpec

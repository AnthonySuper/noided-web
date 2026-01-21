{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Noided.Row.Internal.Type.RowSpec (spec) where

import Noided.Row.Internal.Type.Row
import Noided.Row.Internal.Type.RowElement
import Noided.Row.Internal.Type.RowLabel
import Test.Hspec
import Text.Read

emptyRow :: Row '[]
emptyRow = EmptyRow

singleElementRow :: Row '["foo" :=> Int]
singleElementRow = #foo :==> 10 :::? EmptyRow

twoElementRow :: Row ["bar" :=> String, "foo" :=> Int]
twoElementRow = #bar :==> "yeah" :::? singleElementRow

shouldReadShowRoundtrip :: (Eq a, Read a, Show a) => a -> Expectation
shouldReadShowRoundtrip r =
  readEither (show r) `shouldBe` Right r

readShowSpec :: Spec
readShowSpec = describe "reading/showing" $ do
  it "roundtrips an empty row" $
    shouldReadShowRoundtrip emptyRow
  it "roundtrips a single-element row" $
    shouldReadShowRoundtrip singleElementRow
  it "roundtrips a two-element row" $
    shouldReadShowRoundtrip twoElementRow

eqSpec :: Spec
eqSpec = describe "equality" $ do
  it "is true for empty rows" $
    emptyRow `shouldBe` EmptyRow
  it "is true for single-element rows" $
    singleElementRow `shouldBe` 10 ::: EmptyRow
  it "is true for multiple-element rows" $
    twoElementRow `shouldBe` "yeah" ::: 10 ::: EmptyRow
  it "is true for appended rows" $
    rowAppend twoElementRow twoElementRow
      `shouldBe` "yeah" ::: 10 ::: "yeah" ::: 10 ::: EmptyRow

spec :: Spec
spec = do
  readShowSpec
  eqSpec
  pure ()

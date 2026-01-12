{-# LANGUAGE OverloadedStrings #-}

module Noided.Pathname.Internal.PathTemplateSpec (spec) where

import Data.Either (isLeft)
import Data.GADT.Compare
import Data.Maybe (isJust, isNothing)
import Data.Some.Newtype
import Data.Type.Equality
import Noided.Pathname.Internal.PathTemplate
import Noided.Pathname.Internal.PieceTemplate
import Noided.Pathname.Internal.RouteParams
import Noided.Pathname.Internal.SpecGen
import Test.Hspec
import Test.QuickCheck (forAll, property)
import Text.Read (readMaybe)

showReadSpec :: Spec
showReadSpec = describe "show/read" $ do
  it "can show a single piece" $
    show PathEnd `shouldBe` "PathEnd"
  it "can show two pieces" $
    show (capPiece @Int :/ PathEnd)
      `shouldBe` "CapPiece :/ PathEnd"
  it "can show multiple pieces" $
    show (StaticPiece "foo" :/ capPiece @Int :/ PathEnd)
      `shouldBe` "StaticPiece \"foo\" :/ (CapPiece :/ PathEnd)"
  it "can read multiple pieces" $
    readMaybe "StaticPiece \"foo\" :/ CapPiece :/ PathEnd"
      `shouldBe` Just (StaticPiece "foo" :/ CapPiece @Int :/ PathEnd)

testEqualitySpec :: Spec
testEqualitySpec = describe "testing equality" $ do
  it "does not change testEquality under prepending a static piece" $ property $
    forAll genSomePath $ \r ->
      withSome r $ \path ->
        testEquality path (StaticPiece "wow" :/ path) `shouldSatisfy` isJust
  it "does change testEquality under prepending a cap piece" $ property $
    forAll genSomePath $ \r ->
      withSome r $ \path ->
        testEquality path (capPiece @Int :/ path) `shouldSatisfy` isNothing
  it "changes geq under prepending a static piece" $ property $
    forAll genSomePath $ \r ->
      withSome r $ \path ->
        geq path (StaticPiece "wow" :/ path) `shouldSatisfy` isNothing
  it "does change testEquality under prepending a cap piece" $ property $
    forAll genSomePath $ \r ->
      withSome r $ \path ->
        geq path (capPiece @Int :/ path) `shouldSatisfy` isNothing

orderingSpec :: Spec
orderingSpec = describe "ordering" $ do
  it "is EQ with same params" $ property $
    forAll genSomePath $ \r ->
      r `shouldBe` r
  it "is inverted properly" $ property $
    forAll genSomePath $ \lhs ->
      forAll genSomePath $ \rhs ->
        compare lhs rhs `shouldBe` invertComparison (compare rhs lhs)

matchingSpec :: Spec
matchingSpec = describe "matching" $ do
  describe "with an empty path" $ do
    let f caps = matchPathTemplate caps PathEnd
    it "matches empty" $ f [] `shouldBe` Right RPNil
    it "does not match present" $ f ["foo"] `shouldSatisfy` isLeft
  describe "with a single path" $ do
    let f caps = matchPathTemplate caps (StaticPiece "foo" :/ PathEnd)
    it "does not match empty" $ f [] `shouldSatisfy` isLeft
    it "does not match bad piece" $ f ["bad"] `shouldSatisfy` isLeft
    it "matches good piece" $ f ["foo"] `shouldBe` Right RPNil
  describe "capturing values" $ do
    let f caps = matchPathTemplate caps (StaticPiece "foo" :/ CapPiece @Int :/ PathEnd)
    it "does not match empty" $ f [] `shouldSatisfy` isLeft
    it "does not match bad piece" $ f ["bad"] `shouldSatisfy` isLeft
    it "does not match bad piece + cap" $ f ["bad", "10"] `shouldSatisfy` isLeft
    it "does not good piece + bad cap" $ f ["foo", "bar"] `shouldSatisfy` isLeft
    it "matches a good route" $ f ["foo", "10"] `shouldBe` Right (10 :-$ RPNil)

spec :: Spec
spec = do
  orderingSpec
  testEqualitySpec
  showReadSpec
  matchingSpec
  return ()

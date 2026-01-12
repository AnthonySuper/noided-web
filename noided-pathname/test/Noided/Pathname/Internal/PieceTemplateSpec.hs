{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Pathname.Internal.PieceTemplateSpec (spec) where

import Data.Maybe (isJust, isNothing)
import Data.Type.Equality
import Noided.Pathname.Internal.PieceTemplate
import Noided.Pathname.Internal.SpecGen
import Test.Hspec
import Test.QuickCheck (forAll, property)
import Text.Read (readMaybe)

testEqualitySpec :: Spec
testEqualitySpec = describe "testEquality" $ do
  it "considers two static pieces equal" $
    (StaticPiece "foo" `testEquality` StaticPiece "bar") `shouldSatisfy` isJust
  it "considers two captures equal if types are equal" $
    (capPiece @Int `testEquality` capPiece @Int) `shouldSatisfy` isJust
  it "does not consider a cap piece and a static piece equal" $
    (capPiece @Int `testEquality` StaticPiece "bar") `shouldSatisfy` isNothing

showReadSpec :: Spec
showReadSpec = describe "show/read" $ do
  it "can show a cap piece" $ do
    show (capPiece @Int) `shouldBe` "CapPiece"
  it "can show a static piece" $ do
    show (StaticPiece "foo") `shouldBe` "StaticPiece \"foo\""
  it "can read a static piece" $ do
    readMaybe "StaticPiece \"foo\"" `shouldBe` Just (StaticPiece "foo")

gcompareSpec :: Spec
gcompareSpec = describe "gcompare" $ do
  it "has reflexive equality" $ property $
    forAll genSomePiece $ \r ->
      compare r r `shouldBe` EQ
  it "has symmetry" $ property $
    forAll genSomePiece $ \lhs ->
      forAll genSomePiece $ \rhs ->
        compare lhs rhs `shouldBe` invertComparison (compare rhs lhs)

spec :: Spec
spec = do
  testEqualitySpec
  showReadSpec
  gcompareSpec
  return ()

{-# LANGUAGE OverloadedStrings #-}

module Noided.Pathname.Internal.PathTemplateSpec (spec) where

import Control.Monad (when)
import Data.Either (isLeft)
import Data.GADT.Compare
import Data.Maybe (isJust, isNothing)
import Data.Some.Newtype
import Data.Text (Text)
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
  it "ignores a leading static piece in either direction" $ do
    -- Both of these templates capture '[Int], so both directions must agree.
    testEquality (capPiece @Int :/ PathEnd) (StaticPiece "a" :/ capPiece @Int :/ PathEnd)
      `shouldSatisfy` isJust
    testEquality (StaticPiece "a" :/ capPiece @Int :/ PathEnd) (capPiece @Int :/ PathEnd)
      `shouldSatisfy` isJust
  it "is reflexive" $ property $
    forAll genSomePath $ \r ->
      withSome r $ \path ->
        testEquality path path `shouldSatisfy` isJust
  it "is symmetric" $ property $
    forAll (genPathsWithSameCaptures 2) $ \paths ->
      case paths of
        [sl, sr] ->
          withSome sl $ \lhs ->
            withSome sr $ \rhs ->
              isJust (testEquality lhs rhs) `shouldBe` isJust (testEquality rhs lhs)
        _ -> expectationFailure "expected two generated paths"
  it "is symmetric against unrelated paths" $ property $
    forAll genSomePath $ \sl ->
      forAll genSomePath $ \sr ->
        withSome sl $ \lhs ->
          withSome sr $ \rhs ->
            isJust (testEquality lhs rhs) `shouldBe` isJust (testEquality rhs lhs)
  it "is transitive" $ property $
    forAll (genPathsWithSameCaptures 3) $ \paths ->
      case paths of
        [sa, sb, sc] ->
          withSome sa $ \a ->
            withSome sb $ \b ->
              withSome sc $ \c ->
                when (isJust (testEquality a b) && isJust (testEquality b c)) $
                  testEquality a c `shouldSatisfy` isJust
        _ -> expectationFailure "expected three generated paths"

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
    it "matches a good route with a trailing slash" $
      f ["foo", "10", ""] `shouldBe` Right (10 :-$ RPNil)
    it "does not match pieces after an empty piece" $
      f ["foo", "10", "", "junk"] `shouldSatisfy` isLeft
    it "does not match an empty piece in place of a capture" $
      f ["foo", ""] `shouldSatisfy` isLeft
  describe "multiple captures" $ do
    let template = StaticPiece "users" :/ CapPiece @Int :/ StaticPiece "posts" :/ CapPiece @Int :/ PathEnd
    -- Written without parentheses on purpose: ':-$' has to be right
    -- associative for this to even typecheck.
    it "matches params built without parentheses" $
      matchPathTemplate ["users", "5", "posts", "42"] template
        `shouldBe` Right (5 :-$ 42 :-$ RPNil)

urlRoundTripSpec :: Spec
urlRoundTripSpec = describe "url round-trip" $ do
  it "splits off the leading slash it generates" $
    splitPathPieces (usePathTemplateParams (StaticPiece "users" :/ CapPiece @Int :/ PathEnd) (42 :-$ RPNil))
      `shouldBe` ["users", "42", ""]
  it "escapes a capture that would otherwise split into two pieces" $
    usePathTemplateParams (StaticPiece "users" :/ CapPiece @Text :/ PathEnd) ("a/b" :-$ RPNil)
      `shouldBe` "/users/a%2Fb/"
  it "escapes a static piece" $
    usePathTemplateParams (StaticPiece "a b" :/ PathEnd) RPNil `shouldBe` "/a%20b/"
  it "round-trips the empty template" $
    splitPathPieces (usePathTemplateParams PathEnd RPNil) `shouldBe` []
  it "matches the url it generates" $ property $
    forAll genSomeRoute $ \(SomeRoute template params) ->
      matchPathTemplate (splitPathPieces (usePathTemplateParams template params)) template
        `shouldBe` Right params

spec :: Spec
spec = do
  orderingSpec
  testEqualitySpec
  showReadSpec
  matchingSpec
  urlRoundTripSpec
  return ()

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}

module Noided.Row.Internal.SomeFieldSpec (spec) where

import Data.HKD
import Data.Monoid (Endo (..), Sum (..))
import Data.Type.Equality
import Noided.Row
import Optics.Core
import Test.Hspec
import Type.Reflection

data TermWrapping a where
  StrTerm :: String -> TermWrapping String
  ConstTerm :: String -> TermWrapping a
  IntTerm :: Int -> TermWrapping Int

_IntTerm :: Prism' (TermWrapping Int) Int
_IntTerm = prism' IntTerm f
  where
    f :: TermWrapping Int -> Maybe Int
    f (IntTerm i) = pure i
    f _ = Nothing

type TestRowLabels = ["firstName" :=> String, "lastName" :=> String, "powerLevel" :=> Int, "age" :=> Int]

toIntField :: RowFieldOf labels contained -> Maybe (Lens' (WrappedRow labels wrapper) (wrapper Int))
toIntField r = do
  Refl <- testEquality (rowFieldTypeRep r) (typeRep @Int)
  Just $ rowFieldLens r

intFieldsOf :: WrappedRow labels (RowFieldOf labels) -> Traversal' (WrappedRow labels wrapper) (wrapper Int)
intFieldsOf = (`appEndo` (noIx (castOptic ignored))) . ffoldMap f
  where
    f r = Endo $ \l ->
      case toIntField r of
        Nothing -> l
        Just l' -> adjoin l' l

testRowFields :: Traversal' (WrappedRow TestRowLabels wrapper) (wrapper Int)
testRowFields = intFieldsOf (rowKnownFields @TestRowLabels)

testRow :: WrappedRow TestRowLabels TermWrapping
testRow =
  #firstName :==> StrTerm "rich"
    :::%? #lastName :==> StrTerm "Evans"
    :::%? #powerLevel :==> ConstTerm @Int "boundless"
    :::%? #age :==> IntTerm 100
    :::%? EmptyWrappedRow

knownFieldSpec :: Spec
knownFieldSpec = describe "rowKnownFields" $ do
  it "can work for things" $ do
    let r = rowKnownFields @TestRowLabels
    ffoldMap (const $ Sum (1 :: Int)) r
      `shouldBe` ffoldMap (const $ Sum 1) testRow
  it "can have int fields grabbed" $ do
    toListOf (testRowFields % _IntTerm) testRow `shouldBe` [100]

spec :: Spec
spec = do
  knownFieldSpec

{-# LANGUAGE DataKinds #-}

module Noided.Row.Internal.Type.RowElementSpec (spec) where

import Noided.Row.Internal.Type.RowElement
import Test.Hspec
import Text.Read

unwrapElement :: RowElement "foo" Int -> Int
unwrapElement (MkRowElement e) = e

spec :: Spec
spec = do
  describe "read/show" $ do
    it "can show" $ do
      show (MkRowElement @"foo" (10 :: Int))
        `shouldBe` "#foo :==> 10"
    it "can read" $ do
      let afterRead = readEither "#foo :==> 10"
      fmap unwrapElement afterRead `shouldBe` Right 10
    it "can read a list" $ do
      let afterRead = readEither "[#foo :==> 10, #foo :==> 11]"
      fmap (fmap unwrapElement) afterRead
        `shouldBe` Right [10, 11]
    it "can round-trip" $ do
      let shown = (show $ MkRowElement @"foo" (10 :: Int))
      let afterRead = readEither shown
      fmap unwrapElement afterRead `shouldBe` Right 10

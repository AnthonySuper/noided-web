{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Noided.Web.Html.Internal.Class.DomIdSpec (spec) where

import GHC.Generics
import Noided.Web.Html.Internal.Class.DomId
import Noided.Web.Html.Internal.Type.DomIdWriter
import Test.Hspec

data MyEnum = EnumFoo | EnumBar
  deriving (Generic, Show)

instance DomId MyEnum where
  asDomId = asDomId . from

data MyRec = MyRec { fieldX :: Int, fieldY :: Int }
  deriving (Generic, Show)

instance DomId MyRec where
  asDomId = asDomId . from

data ViaGeneric = ViaGenericA | ViaGenericB
  deriving (Generic, Show)
  deriving (DomId) via (Generically ViaGeneric)

spec :: Spec
spec = describe "DomId Instances" $ do
  describe "Generics" $ do
    it "renders enum constructors" $ do
      domIdToText (asDomId EnumFoo) `shouldBe` "EnumFoo"
      domIdToText (asDomId EnumBar) `shouldBe` "EnumBar"

    it "renders records with constructor name but no field names" $
      domIdToText (asDomId (MyRec 10 20)) `shouldBe` "MyRec--10--20"

    it "renders via DerivingVia Generically" $ do
      domIdToText (asDomId ViaGenericA) `shouldBe` "ViaGenericA"
      domIdToText (asDomId ViaGenericB) `shouldBe` "ViaGenericB"

  describe "Bool" $ do
    it "renders True as true" $
      domIdToText (asDomId True) `shouldBe` "true"
    it "renders False as false" $
      domIdToText (asDomId False) `shouldBe` "false"

  describe "Maybe" $ do
    it "renders Nothing as Nothing" $
      domIdToText (asDomId (Nothing :: Maybe Int)) `shouldBe` "Nothing"
    it "renders Just as Just--value" $
      domIdToText (asDomId (Just (123 :: Int))) `shouldBe` "Just--123"

  describe "Tuples" $ do
    it "renders 4-tuple" $
      domIdToText (asDomId ("a" :: String, "b" :: String, "c" :: String, "d" :: String)) `shouldBe` "a--b--c--d"

  describe "Numerics" $ do
    it "renders Int" $
      domIdToText (asDomId (123 :: Int)) `shouldBe` "123"
    it "renders Double" $
      domIdToText (asDomId (123.45 :: Double)) `shouldBe` "123.45"

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Translate.Internal.Type.ParamsSpec (spec) where

import Data.Aeson (eitherDecodeStrictText, encode, toJSON)
import Data.Aeson qualified as Aeson
import Noided.Translate.Internal.Type.Params
import Test.Hspec

spec :: Spec
spec = describe "TranslateParam JSON" $ do
  it "encodes fragments as strings" $
    toJSON (ParamFragment "hi") `shouldBe` Aeson.String "hi"

  it "encodes numbers as numbers" $ do
    encode (ParamInt 3) `shouldBe` "3"
    encode (ParamFloat 1.5) `shouldBe` "1.5"

  it "decodes strings and integral numbers" $ do
    eitherDecodeStrictText "\"hi\"" `shouldBe` Right (ParamFragment "hi")
    eitherDecodeStrictText "3" `shouldBe` Right (ParamInt 3)

  it "decodes fractional numbers as floats" $
    eitherDecodeStrictText "1.5" `shouldBe` Right (ParamFloat 1.5)

  it "rejects other JSON values" $
    (eitherDecodeStrictText "true" :: Either String TranslateParam) `shouldSatisfy` either (const True) (const False)

  it "encodes params maps as plain objects" $
    toJSON (TranslateParams [("a", ParamInt 1), ("b", ParamFragment "x")])
      `shouldBe` Aeson.object ["a" Aeson..= (1 :: Int), "b" Aeson..= ("x" :: String)]

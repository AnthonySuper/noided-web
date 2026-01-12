{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Noided.Translate.Internal.Type.MessagesSpec (spec) where

import Data.Aeson (eitherDecodeStrictText)
import Data.Map qualified as Map
import Data.Text (Text, pack)
import Noided.Translate.Internal.Type.Message
import Noided.Translate.Internal.Type.Messages
import Test.Hspec
import Text.RawString.QQ

shouldParseJSON ::
  String ->
  Map.Map Text Message ->
  Expectation
shouldParseJSON str m =
  eitherDecodeStrictText (pack str)
    `shouldBe` Right (messagesFromKVMap m)

parsingSpec :: Spec
parsingSpec = do
  it "can parse an empty map" $
    "{}" `shouldParseJSON` mempty
  it "can parse with raw kvs" $
    [r|
    {
      "foo": "bar",
      "foo.baz": "baz"
    }
    |]
      `shouldParseJSON` [("foo", Syn [Fragment "bar"]), ("foo.baz", Syn [Fragment "baz"])]
  it "can parse with nested kvs" $
    [r|
      {
        "foo": {
          "bar": "1",
          "yes": {
            "no": "2",
            "yes": "3"
          }
        }
      }
    |]
      `shouldParseJSON` [ ("foo.bar", Syn [Fragment "1"]),
                          ("foo.yes.no", Syn [Fragment "2"]),
                          ("foo.yes.yes", Syn [Fragment "3"])
                        ]

spec :: Spec
spec = do
  parsingSpec
  return ()

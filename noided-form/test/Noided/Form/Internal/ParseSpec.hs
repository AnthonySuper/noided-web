{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Form.Internal.ParseSpec where

import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Noided.Form.Internal.Parse
import Noided.Form.Internal.Type.FormInputKey
import Noided.Form.Internal.Type.FormSubmission
import Test.Hspec

shouldParseTo :: Text -> FormInputKey -> Expectation
shouldParseTo k v = parseInputKey k `shouldBe` Right v

shouldFailParse :: Text -> Expectation
shouldFailParse k = parseInputKey k `shouldSatisfy` isLeft

inputKeySpec :: Spec
inputKeySpec = describe "parseInputKey" $ do
  it "can parse an empty key" $
    "" `shouldParseTo` mempty
  it "can parse a single key" $
    "foo" `shouldParseTo` pure (TextPiece "foo")
  it "can parse single-level nesting" $
    "foo[bar]" `shouldParseTo` Seq.fromList [TextPiece "foo", TextPiece "bar"]
  it "can parse array nesting (1-level)" $
    "foo[]" `shouldParseTo` Seq.fromList [TextPiece "foo", BracesPiece]
  it "can parse multi-level nesting" $
    "foo[bar][baz]" `shouldParseTo` Seq.fromList [TextPiece "foo", TextPiece "bar", TextPiece "baz"]
  it "can parse mixed nesting" $
    "foo[bar][]" `shouldParseTo` Seq.fromList [TextPiece "foo", TextPiece "bar", BracesPiece]
  it "fails on initial nesting" $
    shouldFailParse "[foo]"
  it "fails on unclosed bracket" $
    shouldFailParse "foo[bar"
  it "fails on content after brackets" $
    shouldFailParse "foo[]bar"
  it "fails on extra closing bracket" $
    shouldFailParse "foo[bar]]"
  it "fails on content after nested key" $
    shouldFailParse "foo[bar]baz"
  it "can parse key starting with bracket inside brackets" $
    "foo[[bar]" `shouldParseTo` Seq.fromList [TextPiece "foo", TextPiece "[bar"]

spec :: Spec
spec = do
  inputKeySpec
  
  describe "fromTextKeysAndValues" $ do
    it "parses valid keys and ignores invalid ones" $ do
      let inputs =
            [ ("name", Just (TextValue "Alice")),
              ("invalid[", Just (TextValue "IgnoreMe")),
              ("age", Just (TextValue "30"))
            ]
      let expected =
            SubmissionObject $
              Map.fromList
                [ ("name", SubmissionValue (TextValue "Alice")),
                  ("age", SubmissionValue (TextValue "30"))
                ]
      fromTextKeysAndValues inputs `shouldBe` expected

  describe "fromTextKeysAndValuesStrict" $ do
    it "parses all valid keys" $ do
      let inputs =
            [ ("name", Just (TextValue "Alice")),
              ("age", Just (TextValue "30"))
            ]
      let expected =
            Right $
              SubmissionObject $
                Map.fromList
                  [ ("name", SubmissionValue (TextValue "Alice")),
                    ("age", SubmissionValue (TextValue "30"))
                  ]
      fromTextKeysAndValuesStrict inputs `shouldBe` expected

    it "fails on invalid key" $ do
      let inputs =
            [ ("name", Just (TextValue "Alice")),
              ("invalid[", Just (TextValue "IgnoreMe"))
            ]
      fromTextKeysAndValuesStrict inputs `shouldSatisfy` isLeft

  describe "fromKeysAndValues" $ do
    it "parses simple flat keys" $ do
      let inputs =
            [ (Seq.fromList [TextPiece "name"], Just (TextValue "Alice")),
              (Seq.fromList [TextPiece "age"], Just (TextValue "30"))
            ]
      let expected =
            SubmissionObject $
              Map.fromList
                [ ("name", SubmissionValue (TextValue "Alice")),
                  ("age", SubmissionValue (TextValue "30"))
                ]
      fromKeysAndValues inputs `shouldBe` expected

    it "parses nested objects" $ do
      let inputs =
            [ (Seq.fromList [TextPiece "user", TextPiece "name"], Just (TextValue "Alice")),
              (Seq.fromList [TextPiece "user", TextPiece "address", TextPiece "city"], Just (TextValue "New York"))
            ]
      let expected =
            SubmissionObject $
              Map.fromList
                [ ( "user",
                    SubmissionObject $
                      Map.fromList
                        [ ("name", SubmissionValue (TextValue "Alice")),
                          ( "address",
                            SubmissionObject $
                              Map.fromList [("city", SubmissionValue (TextValue "New York"))]
                          )
                        ]
                  )
                ]
      fromKeysAndValues inputs `shouldBe` expected

    it "handles array append []" $ do
      let inputs =
            [ (Seq.fromList [TextPiece "tags", BracesPiece], Just (TextValue "haskell")),
              (Seq.fromList [TextPiece "tags", BracesPiece], Just (TextValue "functional"))
            ]
      let expected =
            SubmissionObject $
              Map.fromList
                [ ( "tags",
                    SubmissionArray $
                      Seq.fromList
                        [ SubmissionValue (TextValue "haskell"),
                          SubmissionValue (TextValue "functional")
                        ]
                  )
                ]
      fromKeysAndValues inputs `shouldBe` expected

    it "merges objects in arrays (smart merge)" $ do
      let inputs =
            [ (Seq.fromList [TextPiece "users", BracesPiece, TextPiece "name"], Just (TextValue "Alice")),
              (Seq.fromList [TextPiece "users", BracesPiece, TextPiece "email"], Just (TextValue "alice@example.com")),
              (Seq.fromList [TextPiece "users", BracesPiece, TextPiece "name"], Just (TextValue "Bob")),
              (Seq.fromList [TextPiece "users", BracesPiece, TextPiece "email"], Just (TextValue "bob@example.com"))
            ]
      let expected =
            SubmissionObject $
              Map.fromList
                [ ( "users",
                    SubmissionArray $
                      Seq.fromList
                        [ SubmissionObject $
                            Map.fromList
                              [ ("name", SubmissionValue (TextValue "Alice")),
                                ("email", SubmissionValue (TextValue "alice@example.com"))
                              ],
                          SubmissionObject $
                            Map.fromList
                              [ ("name", SubmissionValue (TextValue "Bob")),
                                ("email", SubmissionValue (TextValue "bob@example.com"))
                              ]
                        ]
                  )
                ]
      fromKeysAndValues inputs `shouldBe` expected

    it "handles deep nesting with arrays and objects" $ do
      -- items[][tags][]
      let inputs =
            [ (Seq.fromList [TextPiece "users", BracesPiece, TextPiece "name"], Just (TextValue "Mike")),
              (Seq.fromList [TextPiece "users", BracesPiece, TextPiece "tags", BracesPiece], Just (TextValue "hack")),
              (Seq.fromList [TextPiece "users", BracesPiece, TextPiece "tags", BracesPiece], Just (TextValue "fraud")),
              (Seq.fromList [TextPiece "users", BracesPiece, TextPiece "name"], Just (TextValue "Jay")),
              (Seq.fromList [TextPiece "users", BracesPiece, TextPiece "tags", BracesPiece], Just (TextValue "hack"))
            ]

      let expected =
            SubmissionObject $
              Map.fromList
                [ ( "users",
                    SubmissionArray $
                      Seq.fromList
                        [ SubmissionObject $
                            Map.fromList
                              [ ("name", SubmissionValue (TextValue "Mike")),
                                ( "tags",
                                  SubmissionArray $
                                    Seq.fromList [SubmissionValue (TextValue "hack"), SubmissionValue (TextValue "fraud")]
                                )
                              ],
                          SubmissionObject $ -- Split happened here because 'tags' existed
                            Map.fromList
                              [ ("name", SubmissionValue (TextValue "Jay")),
                                ("tags", SubmissionArray $ Seq.fromList [SubmissionValue (TextValue "hack")])
                              ]
                        ]
                  )
                ]

      fromKeysAndValues inputs `shouldBe` expected

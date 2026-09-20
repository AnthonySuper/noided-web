{-# LANGUAGE OverloadedStrings #-}

module Noided.Translate.Internal.Type.MessageSpec (spec) where

import Data.Either (isLeft)
import Data.Text (Text)
import Noided.Translate.Internal.Type.Message
import Test.Hspec

shouldParseTo ::
  Text ->
  [Message] ->
  Expectation
shouldParseTo t m =
  parseMessage t `shouldBe` Right (Syn m)

shouldFailToParse :: Text -> Expectation
shouldFailToParse t =
  parseMessage t `shouldSatisfy` isLeft

shouldSimplifyTo :: [Message] -> [Message] -> Expectation
shouldSimplifyTo lhs rhs = simplifySyn lhs `shouldBe` rhs

simplifySpec :: Spec
simplifySpec = describe "simplification" $ do
  it "can simplify two fragments" $
    [Fragment "foo", Fragment "bar"] `shouldSimplifyTo` [Fragment "foobar"]
  it "can simplify nested syns" $
    [Syn [Fragment "foo"]] `shouldSimplifyTo` [Fragment "foo"]
  it "can apply further simplification to nested syns" $
    [Fragment "foo", Syn [Fragment "bar"]] `shouldSimplifyTo` [Fragment "foobar"]

parsingSpec :: Spec
parsingSpec = describe "parsing" $ do
  it "can parse a single fragment" $
    "foo" `shouldParseTo` [Fragment "foo"]
  it "can parse a var-then-fragment" $ do
    "foo $bar" `shouldParseTo` [Fragment "foo ", Var "bar"]
  it "can parse an escaped $" $ do
    "foo $$bar" `shouldParseTo` [Fragment "foo $bar"]
  it "can parse a calc" $ do
    "foo {pluralize ( $bar ) { default { yeah } }}"
      `shouldParseTo` [Fragment "foo ", Calc (Pluralize "bar" [] (Syn [Fragment "yeah "]))]

-- | Messages that used to parse as a silently truncated prefix, because
-- 'Data.Attoparsec.Text.parseOnly' is happy to leave input unconsumed.
truncationSpec :: Spec
truncationSpec = describe "leftover input" $ do
  it "rejects a trailing bare $ instead of dropping it" $
    shouldFailToParse "Cost: 5$"
  it "says something useful about a trailing bare $" $
    parseMessage "Cost: 5$"
      `shouldBe` Left "parse: unexpected input at character 7 of message \"Cost: 5$\": \"$\""
  it "rejects an unescaped brace instead of truncating the message" $
    shouldFailToParse "Save {50%}"
  it "rejects an unescaped closing brace" $
    shouldFailToParse "Save 50%}"
  it "rejects an unknown pluralization form" $
    shouldFailToParse "{pluralize ($n) { few { a few } default { none } }}"
  it "rejects a pluralize with no default clause" $
    shouldFailToParse "{pluralize ($n) { one { one } }}"
  it "rejects an unterminated calc" $
    shouldFailToParse "{pluralize ($n) { default { none } }"

escapeSpec :: Spec
escapeSpec = describe "escapes" $ do
  it "can parse an escaped opening brace" $
    "Save {{50%" `shouldParseTo` [Fragment "Save {50%"]
  it "can parse an escaped closing brace" $
    "Save 50%}}" `shouldParseTo` [Fragment "Save 50%}"]
  it "can parse a brace-wrapped literal" $
    "Save {{50%}}" `shouldParseTo` [Fragment "Save {50%}"]
  it "can parse every escape alongside a real variable" $
    "$$ {{ }} $count!"
      `shouldParseTo` [Fragment "$ { } ", Var "count", Fragment "!"]
  it "can parse an escaped opening brace inside a calc" $
    "{pluralize ($n) { default { {{none } }}"
      `shouldParseTo` [Calc (Pluralize "n" [] (Syn [Fragment "{", Fragment "none "]))]

defaultPositionSpec :: Spec
defaultPositionSpec = describe "the position of the default clause" $ do
  let expected =
        [ Calc $
            Pluralize
              "n"
              [(One, Syn [Fragment "one "]), (Many, Syn [Fragment "lots "])]
              (Syn [Fragment "none "])
        ]
  it "can parse a default clause in the last position" $
    "{pluralize ($n) { one { one } many { lots } default { none } }}"
      `shouldParseTo` expected
  it "can parse a default clause in the middle position" $
    "{pluralize ($n) { one { one } default { none } many { lots } }}"
      `shouldParseTo` expected
  it "can parse a default clause in the first position" $
    "{pluralize ($n) { default { none } one { one } many { lots } }}"
      `shouldParseTo` expected
  it "uses the first default clause when there is more than one" $
    "{pluralize ($n) { default { none } default { other } }}"
      `shouldParseTo` [Calc (Pluralize "n" [] (Syn [Fragment "none "]))]

-- | Messages taken verbatim from @optimize-beer/config/translations/en@, so
-- that a change to the parser cannot quietly invalidate a real translation
-- file (which 'Noided.Web.Internal.Effect.Translate.loadFile' would then drop
-- wholesale).
realWorldSpec :: Spec
realWorldSpec = describe "real translation strings" $ do
  it "can parse errors.NotEnoughReqChars" $
    "Missing required characters in category: $category (need at least $minAmount)."
      `shouldParseTo` [ Fragment "Missing required characters in category: ",
                        Var "category",
                        Fragment " (need at least ",
                        Var "minAmount",
                        Fragment ")."
                      ]
  it "can parse form.password_policy.min_length" $
    "{pluralize($count) { one { At least $count character long } default { At least $count characters long } }}"
      `shouldParseTo` [ Calc $
                          Pluralize
                            "count"
                            [ ( One,
                                Syn
                                  [ Fragment "At least ",
                                    Var "count",
                                    Fragment " character long "
                                  ]
                              )
                            ]
                            ( Syn
                                [ Fragment "At least ",
                                  Var "count",
                                  Fragment " characters long "
                                ]
                            )
                      ]

spec :: Spec
spec = do
  simplifySpec
  parsingSpec
  truncationSpec
  escapeSpec
  defaultPositionSpec
  realWorldSpec

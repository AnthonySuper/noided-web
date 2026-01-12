{-# LANGUAGE OverloadedStrings #-}

module Noided.Translate.Internal.Type.MessageSpec (spec) where

import Data.Text (Text)
import Noided.Translate.Internal.Type.Message
import Test.Hspec

shouldParseTo ::
  Text ->
  [Message] ->
  Expectation
shouldParseTo t m =
  parseMessage t `shouldBe` Right (Syn m)

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

spec :: Spec
spec = do
  simplifySpec
  parsingSpec

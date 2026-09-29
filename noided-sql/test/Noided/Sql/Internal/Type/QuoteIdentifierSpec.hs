{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Type.QuoteIdentifierSpec (spec) where

import Noided.Sql.Internal.Type.QuoteIdentifier
import Test.Hspec

spec :: Spec
spec = describe "quoteIdentifierIfNeeded" $ do
  it "leaves plain lowercase identifiers bare" $ do
    quoteIdentifierIfNeeded "id" `shouldBe` "id"
    quoteIdentifierIfNeeded "name" `shouldBe` "name"
    quoteIdentifierIfNeeded "_private" `shouldBe` "_private"
    quoteIdentifierIfNeeded "col_1$x" `shouldBe` "col_1$x"
    quoteIdentifierIfNeeded "users_1" `shouldBe` "users_1"

  it "leaves unreserved keywords bare" $ do
    quoteIdentifierIfNeeded "value" `shouldBe` "value"
    quoteIdentifierIfNeeded "key" `shouldBe` "key"

  it "quotes reserved and other non-unreserved keywords" $ do
    quoteIdentifierIfNeeded "user" `shouldBe` "\"user\""
    quoteIdentifierIfNeeded "order" `shouldBe` "\"order\""
    quoteIdentifierIfNeeded "select" `shouldBe` "\"select\""
    quoteIdentifierIfNeeded "table" `shouldBe` "\"table\""
    -- column-name / type-function keywords are also quoted
    quoteIdentifierIfNeeded "timestamp" `shouldBe` "\"timestamp\""
    quoteIdentifierIfNeeded "left" `shouldBe` "\"left\""

  it "quotes identifiers that would be folded or misparsed" $ do
    quoteIdentifierIfNeeded "userId" `shouldBe` "\"userId\""
    quoteIdentifierIfNeeded "has space" `shouldBe` "\"has space\""
    quoteIdentifierIfNeeded "1abc" `shouldBe` "\"1abc\""
    quoteIdentifierIfNeeded "$abc" `shouldBe` "\"$abc\""
    quoteIdentifierIfNeeded "caf\233" `shouldBe` "\"caf\233\""
    quoteIdentifierIfNeeded "a-b" `shouldBe` "\"a-b\""
    quoteIdentifierIfNeeded "" `shouldBe` "\"\""

  it "doubles embedded quotes" $ do
    quoteIdentifierIfNeeded "we\"ird" `shouldBe` "\"we\"\"ird\""

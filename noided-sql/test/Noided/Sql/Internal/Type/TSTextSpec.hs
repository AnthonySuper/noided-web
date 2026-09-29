{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Type.TSTextSpec (spec) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (digitToInt)
import Noided.Sql.Internal.Type.TSText
import Test.Hspec

-- | Parse a hex string (spaces ignored) into bytes.
hex :: String -> ByteString
hex = BS.pack . go . filter (/= ' ')
  where
    go (a : b : rest) = fromIntegral (digitToInt a * 16 + digitToInt b) : go rest
    go _ = []

-- The payloads below were captured from a real Postgres with @COPY ... WITH (FORMAT binary)@.
spec :: Spec
spec = do
  describe "decodeTSVectorText" $ do
    it "decodes lexemes, positions and weights" $
      decodeTSVectorText
        (hex "00000003 6100 0002c0020003 6974277300 000280040005 7800 00010001")
        `shouldBe` Right "'a':2A,3 'it''s':4B,5 'x':1"

    it "decodes an empty tsvector" $
      decodeTSVectorText (hex "00000000") `shouldBe` Right ""

    it "fails on truncated input" $
      decodeTSVectorText (hex "000000036100") `shouldSatisfy` either (const True) (const False)

  describe "decodeTSQueryText" $ do
    it "decodes operators, negation, phrases, prefixes and weights" $
      decodeTSQueryText
        ( hex
            "0000000c 0203 0202 02040003 0100007100 0100007000 02040001 0100007900 0100007800 0202 0201 0100006361 7400 0108016661 7400"
        )
        `shouldBe` Right "'fat':*A & !'cat' | 'x' <-> 'y' & 'p' <3> 'q'"

    it "parenthesizes lower-precedence children" $
      decodeTSQueryText
        (hex "00000009 0202 0203 0100006500 0100006400 0202 0100006300 0203 0100006200 0100006100")
        `shouldBe` Right "('a' | 'b') & 'c' & ('d' | 'e')"

    it "decodes an empty tsquery" $
      decodeTSQueryText (hex "00000000") `shouldBe` Right ""

{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Type.TSBinarySpec (spec) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (digitToInt)
import Noided.Sql.Internal.Type.PGFullTextSearchWeight
import Noided.Sql.Internal.Type.TSBinary
import Noided.Sql.Internal.Type.TSValue
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
  describe "decodeTSVector" $ do
    it "decodes lexemes, positions and weights" $
      decodeTSVector
        (hex "00000003 6100 0002c0020003 6974277300 000280040005 7800 00010001")
        `shouldBe` Right
          ( TSVector
              [ ("a", [(2, WeightA), (3, WeightD)]),
                ("it's", [(4, WeightB), (5, WeightD)]),
                ("x", [(1, WeightD)])
              ]
          )

    it "decodes an empty tsvector" $
      decodeTSVector (hex "00000000") `shouldBe` Right (TSVector [])

    it "fails on truncated input" $
      decodeTSVector (hex "000000036100") `shouldSatisfy` either (const True) (const False)

  describe "decodeTSQuery" $ do
    -- 'fat':*A & !'cat' | 'x' <-> 'y' & 'p' <3> 'q'
    it "decodes operators, negation, phrases, prefixes and weights" $
      decodeTSQuery
        ( hex
            "0000000c 0203 0202 02040003 0100007100 0100007000 02040001 0100007900 0100007800 0202 0201 0100006361 7400 0108016661 7400"
        )
        `shouldBe` Right
          ( TSOr
              (TSAnd (TSLexeme "fat" [WeightA] True) (TSNot (TSLexeme "cat" [] False)))
              ( TSAnd
                  (TSPhrase 1 (TSLexeme "x" [] False) (TSLexeme "y" [] False))
                  (TSPhrase 3 (TSLexeme "p" [] False) (TSLexeme "q" [] False))
              )
          )

    -- ('a' | 'b') & 'c' & ('d' | 'e')
    it "preserves nesting" $
      decodeTSQuery
        (hex "00000009 0202 0203 0100006500 0100006400 0202 0100006300 0203 0100006200 0100006100")
        `shouldBe` Right
          ( TSAnd
              (TSAnd (TSOr (TSLexeme "a" [] False) (TSLexeme "b" [] False)) (TSLexeme "c" [] False))
              (TSOr (TSLexeme "d" [] False) (TSLexeme "e" [] False))
          )

    it "decodes an empty tsquery" $
      decodeTSQuery (hex "00000000") `shouldBe` Right TSEmpty

    it "fails on truncated input" $
      decodeTSQuery (hex "00000001 02") `shouldSatisfy` either (const True) (const False)

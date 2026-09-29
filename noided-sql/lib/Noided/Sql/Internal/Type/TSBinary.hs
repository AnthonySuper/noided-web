{-# LANGUAGE OverloadedStrings #-}

-- | Decoders for Postgres's binary @tsvector@ and @tsquery@ wire formats.
module Noided.Sql.Internal.Type.TSBinary
  ( decodeTSVector,
    decodeTSQuery,
  )
where

import Control.Monad (replicateM)
import Data.Binary.Get
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word
import Noided.Sql.Internal.Type.PGFullTextSearchWeight
import Noided.Sql.Internal.Type.TSValue

cstring :: Get Text
cstring = do
  str <- BL.toStrict <$> getLazyByteStringNul
  either (fail . show) pure (TE.decodeUtf8' str)

runDecoder :: Get a -> ByteString -> Either Text a
runDecoder g bs = case runGetOrFail g (BL.fromStrict bs) of
  Left (_, _, err) -> Left (T.pack err)
  Right (_, _, a) -> Right a

-- | Decode the binary @tsvector@ format.
decodeTSVector :: ByteString -> Either Text TSVector
decodeTSVector = runDecoder $ do
  n <- getInt32be
  TSVector <$> replicateM (fromIntegral n) entry
  where
    entry = do
      lexeme <- cstring
      npos <- getWord16be
      positions <- replicateM (fromIntegral npos) (position <$> getWord16be)
      pure (lexeme, positions)
    -- The top two bits are the weight, the rest is the position.
    position p = (p .&. 0x3FFF, weightFromBits (p `shiftR` 14))
    weightFromBits :: Word16 -> PGFullTextSearchWeight
    weightFromBits 3 = WeightA
    weightFromBits 2 = WeightB
    weightFromBits 1 = WeightC
    weightFromBits _ = WeightD

-- | Decode the binary @tsquery@ format.
-- The wire format is prefix-ordered, with the right operand of a binary operator first.
decodeTSQuery :: ByteString -> Either Text TSQuery
decodeTSQuery = runDecoder $ do
  n <- getInt32be
  if n == 0 then pure TSEmpty else node
  where
    node :: Get TSQuery
    node = do
      ty <- getWord8
      case ty of
        1 -> do
          weight <- getWord8
          prefix <- getWord8
          lexeme <- cstring
          let ws = [w | (b, w) <- [(3, WeightA), (2, WeightB), (1, WeightC), (0, WeightD)], testBit weight b]
          pure (TSLexeme lexeme ws (prefix /= 0))
        2 -> do
          op <- getWord8
          case op of
            1 -> TSNot <$> node
            2 -> binary TSAnd
            3 -> binary TSOr
            4 -> getWord16be >>= binary . TSPhrase
            _ -> fail "unknown tsquery operator"
        _ -> fail "unknown tsquery item type"
    binary f = do
      right <- node
      left <- node
      pure (f left right)

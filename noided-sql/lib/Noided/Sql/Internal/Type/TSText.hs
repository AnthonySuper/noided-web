{-# LANGUAGE OverloadedStrings #-}

-- | Decoders that render Postgres's binary @tsvector@ and @tsquery@ wire formats as text.
module Noided.Sql.Internal.Type.TSText
  ( decodeTSVectorText,
    decodeTSQueryText,
  )
where

import Data.Binary.Get
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word

cstring :: Get Text
cstring = do
  str <- BL.toStrict <$> getLazyByteStringNul
  either (fail . show) pure (TE.decodeUtf8' str)

runDecoder :: Get a -> ByteString -> Either Text a
runDecoder g bs = case runGetOrFail g (BL.fromStrict bs) of
  Left (_, _, err) -> Left (T.pack err)
  Right (_, _, a) -> Right a

quoteLexeme :: Text -> Text
quoteLexeme t = "'" <> T.replace "'" "''" (T.replace "\\" "\\\\" t) <> "'"

weightLetter :: Word16 -> Text
weightLetter 3 = "A"
weightLetter 2 = "B"
weightLetter 1 = "C"
weightLetter _ = ""

-- | Render the binary @tsvector@ format, e.g. @'a':1,2A 'b':3@.
decodeTSVectorText :: ByteString -> Either Text Text
decodeTSVectorText = runDecoder $ do
  n <- getInt32be
  entries <- mapM (const entry) [1 .. n]
  pure (T.unwords entries)
  where
    entry = do
      lexeme <- cstring
      npos <- getWord16be
      positions <- mapM (const getWord16be) [1 .. npos]
      let render p = T.pack (show (p .&. 0x3FFF)) <> weightLetter (p `shiftR` 14)
          suffix = if null positions then "" else ":" <> T.intercalate "," (map render positions)
      pure (quoteLexeme lexeme <> suffix)

data Op = OpNot | OpAnd | OpOr | OpPhrase Word16

opPrec :: Op -> Int
opPrec OpOr = 1
opPrec OpAnd = 2
opPrec (OpPhrase _) = 3
opPrec OpNot = 4

-- | Render the binary @tsquery@ format, e.g. @'a' & 'b':*A@.
-- The wire format is prefix-ordered, with the right operand of a binary operator first.
decodeTSQueryText :: ByteString -> Either Text Text
decodeTSQueryText = runDecoder $ do
  n <- getInt32be
  if n == 0 then pure "" else fst <$> node
  where
    -- Returns rendered text along with the precedence of its top-level operator (5 for atoms).
    node :: Get (Text, Int)
    node = do
      ty <- getWord8
      case ty of
        1 -> do
          weight <- getWord8
          prefix <- getWord8
          lexeme <- cstring
          let ws = T.concat [l | (bit', l) <- [(3, "A"), (2, "B"), (1, "C"), (0, "D")], testBit weight bit']
              mods = (if prefix /= 0 then "*" else "") <> ws
              suffix = if T.null mods then "" else ":" <> mods
          pure (quoteLexeme lexeme <> suffix, 5)
        2 -> do
          opByte <- getWord8
          op <- case opByte of
            1 -> pure OpNot
            2 -> pure OpAnd
            3 -> pure OpOr
            4 -> OpPhrase <$> getWord16be
            _ -> fail "unknown tsquery operator"
          case op of
            OpNot -> do
              (child, cp) <- node
              pure ("!" <> paren (cp < 5) child, opPrec OpNot)
            _ -> do
              (right, rp) <- node
              (left, lp) <- node
              let p = opPrec op
                  isPhrase = case op of OpPhrase _ -> True; _ -> False
                  symbol = case op of
                    OpAnd -> " & "
                    OpOr -> " | "
                    OpPhrase 1 -> " <-> "
                    OpPhrase d -> " <" <> T.pack (show d) <> "> "
              pure (paren (lp < p) left <> symbol <> paren (rp < p || (rp == p && isPhrase)) right, p)
        _ -> fail "unknown tsquery item type"
    paren True t = "(" <> t <> ")"
    paren False t = t

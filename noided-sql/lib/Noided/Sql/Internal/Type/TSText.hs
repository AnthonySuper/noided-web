{-# LANGUAGE OverloadedStrings #-}

-- | Decoders that render Postgres's binary @tsvector@ and @tsquery@ wire formats as text.
module Noided.Sql.Internal.Type.TSText
  ( decodeTSVectorText,
    decodeTSQueryText,
  )
where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word

-- | A minimal binary parser.
newtype P a = P {runP :: ByteString -> Either Text (a, ByteString)}

instance Functor P where
  fmap f (P g) = P $ \bs -> fmap (\(a, r) -> (f a, r)) (g bs)

instance Applicative P where
  pure a = P $ \bs -> Right (a, bs)
  P f <*> P g = P $ \bs -> do
    (h, r) <- f bs
    (a, r') <- g r
    pure (h a, r')

instance Monad P where
  P g >>= f = P $ \bs -> do
    (a, r) <- g bs
    runP (f a) r

failP :: Text -> P a
failP e = P $ \_ -> Left e

word8 :: P Word8
word8 = P $ \bs -> case BS.uncons bs of
  Nothing -> Left "unexpected end of input"
  Just (w, r) -> Right (w, r)

word16 :: P Word16
word16 = do
  a <- word8
  b <- word8
  pure $ (fromIntegral a `shiftL` 8) .|. fromIntegral b

int32 :: P Int32
int32 = do
  a <- word16
  b <- word16
  pure $ fromIntegral ((fromIntegral a `shiftL` 16 :: Word32) .|. fromIntegral b)

cstring :: P Text
cstring = P $ \bs ->
  let (str, rest) = BS.break (== 0) bs
   in case BS.uncons rest of
        Nothing -> Left "unterminated string"
        Just (_, rest') -> case TE.decodeUtf8' str of
          Left err -> Left (T.pack (show err))
          Right t -> Right (t, rest')

quoteLexeme :: Text -> Text
quoteLexeme t = "'" <> T.replace "'" "''" (T.replace "\\" "\\\\" t) <> "'"

weightLetter :: Word16 -> Text
weightLetter 3 = "A"
weightLetter 2 = "B"
weightLetter 1 = "C"
weightLetter _ = ""

-- | Render the binary @tsvector@ format, e.g. @'a':1,2A 'b':3@.
decodeTSVectorText :: ByteString -> Either Text Text
decodeTSVectorText bs = fmap fst $ flip runP bs $ do
  n <- int32
  entries <- mapM (const entry) [1 .. n]
  pure (T.unwords entries)
  where
    entry = do
      lexeme <- cstring
      npos <- word16
      positions <- mapM (const word16) [1 .. npos]
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
decodeTSQueryText bs = fmap fst $ flip runP bs $ do
  n <- int32
  if n == 0 then pure "" else fst <$> node
  where
    -- Returns rendered text along with the precedence of its top-level operator (5 for atoms).
    node :: P (Text, Int)
    node = do
      ty <- word8
      case ty of
        1 -> do
          weight <- word8
          prefix <- word8
          lexeme <- cstring
          let ws = T.concat [l | (bit', l) <- [(3, "A"), (2, "B"), (1, "C"), (0, "D")], testBit weight bit']
              mods = (if prefix /= 0 then "*" else "") <> ws
              suffix = if T.null mods then "" else ":" <> mods
          pure (quoteLexeme lexeme <> suffix, 5)
        2 -> do
          opByte <- word8
          op <- case opByte of
            1 -> pure OpNot
            2 -> pure OpAnd
            3 -> pure OpOr
            4 -> OpPhrase <$> word16
            _ -> failP "unknown tsquery operator"
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
                    OpNot -> "!"
              pure (paren (lp < p) left <> symbol <> paren (rp < p || (rp == p && isPhrase)) right, p)
        _ -> failP "unknown tsquery item type"
    paren True t = "(" <> t <> ")"
    paren False t = t

{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Type.Interval
  ( Interval (..),
    intervalFromDiffTime,
    intervalToBinary,
    intervalFromBinary,
    intervalToText,
  )
where

import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int
import Data.Text (Text, pack)
import Data.Time (DiffTime, diffTimeToPicoseconds)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)

-- | A PostgreSQL @interval@, stored the way Postgres stores it: separate months, days and microseconds.
-- Months and days are not a fixed number of microseconds, so this cannot be a 'DiffTime'.
data Interval = Interval
  { intervalMonths :: !Int32,
    intervalDays :: !Int32,
    intervalMicroseconds :: !Int64
  }
  deriving (Show, Read, Eq, Ord, Generic)

-- | Build an 'Interval' with only a microseconds component (truncating below microsecond precision).
intervalFromDiffTime :: DiffTime -> Interval
intervalFromDiffTime dt = Interval 0 0 (fromIntegral (diffTimeToPicoseconds dt `div` 1000000))

-- | Binary wire format: int64 microseconds, int32 days, int32 months (all big endian).
intervalToBinary :: Interval -> ByteString
intervalToBinary (Interval m d us) =
  BS.pack (be 8 (fromIntegral us) <> be 4 (fromIntegral d) <> be 4 (fromIntegral m))
  where
    be :: Int -> Word64 -> [Word8]
    be n w = [fromIntegral (w `shiftR` (8 * i)) | i <- [n - 1, n - 2 .. 0]]

intervalFromBinary :: ByteString -> Either Text Interval
intervalFromBinary bs
  | BS.length bs /= 16 = Left ("invalid interval: expected 16 bytes, got " <> pack (show (BS.length bs)))
  | otherwise =
      Right $
        Interval
          (fromIntegral (word 12 4))
          (fromIntegral (word 8 4))
          (fromIntegral (word 0 8))
  where
    word :: Int -> Int -> Word64
    word off n = BS.foldl' (\acc b -> (acc `shiftL` 8) .|. fromIntegral b) 0 (BS.take n (BS.drop off bs))

-- | Text rendering that Postgres will accept as an interval literal.
intervalToText :: Interval -> Text
intervalToText (Interval m d us) =
  pack (show m <> " months " <> show d <> " days " <> show us <> " microseconds")

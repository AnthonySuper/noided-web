{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Class.AsHaskellValue where

import Data.Int
import Data.Kind
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time
import Data.Typeable
import Data.UUID (UUID)
import Hasql.Decoders qualified as Dec

type AsHaskellValue :: Type -> Constraint
class (Typeable pgType, Typeable (HaskellTypeOf pgType)) => AsHaskellValue pgType where
  type HaskellTypeOf pgType :: Type
  type HaskellTypeOf pgType = pgType
  decodeHaskellValue :: proxy pgType -> Dec.Value (HaskellTypeOf pgType)

instance AsHaskellValue Scientific where
  decodeHaskellValue _ = Dec.numeric

instance AsHaskellValue Bool where
  decodeHaskellValue _ = Dec.bool

instance AsHaskellValue Text where
  decodeHaskellValue _ = Dec.text

instance AsHaskellValue Int16 where
  decodeHaskellValue _ = Dec.int2

instance AsHaskellValue Int32 where
  decodeHaskellValue _ = Dec.int4

instance AsHaskellValue Int64 where
  decodeHaskellValue _ = Dec.int8

instance AsHaskellValue Int where
  decodeHaskellValue _ = fromIntegral <$> Dec.int8

instance AsHaskellValue Float where
  decodeHaskellValue _ = Dec.float4

instance AsHaskellValue Double where
  decodeHaskellValue _ = Dec.float8

instance AsHaskellValue UUID where
  decodeHaskellValue _ = Dec.uuid

instance AsHaskellValue Day where
  decodeHaskellValue _ = Dec.date

instance AsHaskellValue TimeOfDay where
  decodeHaskellValue _ = Dec.time

instance AsHaskellValue UTCTime where
  decodeHaskellValue _ = Dec.timestamptz

instance AsHaskellValue LocalTime where
  decodeHaskellValue _ = Dec.timestamp

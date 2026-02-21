{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Class.AsHaskellValue where

import Data.Int
import Data.Kind
import Data.Proxy
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time
import Data.Typeable
import Data.UUID (UUID)
import Data.Vector (Vector)
import Hasql.Decoders qualified as Dec
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.PGArray
import Noided.Sql.Internal.Type.SqlType

type AsHaskellValue :: Type -> Constraint
class (Typeable pgType, Typeable (HaskellTypeOf pgType)) => AsHaskellValue pgType where
  type HaskellTypeOf pgType :: Type
  type HaskellTypeOf pgType = pgType
  decodeHaskellValue :: Proxy pgType -> Dec.Value (HaskellTypeOf pgType)

type HaskellValueType :: SqlType -> Type
type family HaskellValueType sqlT where
  HaskellValueType (SqlT Nullable pgType) = Maybe (HaskellTypeOf pgType)
  HaskellValueType (SqlT NonNull pgType) = HaskellTypeOf pgType

instance
  (KnownNullability n, AsHaskellValue pgt, Typeable (HaskellValueType (SqlT n pgt))) =>
  AsHaskellValue (PGArray (SqlT n pgt))
  where
  type HaskellTypeOf (PGArray (SqlT n pgt)) = Vector (HaskellValueType (SqlT n pgt))
  decodeHaskellValue _ =
    case nullabilityS @n of
      NonNullSing -> Dec.vectorArray (Dec.nonNullable (decodeHaskellValue (Proxy @pgt)))
      NullableSing -> Dec.vectorArray (Dec.nullable (decodeHaskellValue (Proxy @pgt)))

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

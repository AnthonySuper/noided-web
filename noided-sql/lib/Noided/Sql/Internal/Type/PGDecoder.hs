module Noided.Sql.Internal.Type.PGDecoder where

import Data.HKD
import Data.Kind
import Data.Proxy
import Hasql.Decoders qualified as Dec
import Noided.Sql.Internal.Class.AsHaskellValue
import Noided.Sql.Internal.Type.HaskellT
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlType

type PGDecoder :: SqlType -> Type
data PGDecoder sqlT where
  DecodePG ::
    (KnownNullability nullability, AsHaskellValue pgType) =>
    PGDecoder (SqlT nullability pgType)

pgDecoder :: (KnownNullability nullability, AsHaskellValue pgType) => PGDecoder (SqlT nullability pgType)
pgDecoder = DecodePG

nullifyPG :: PGDecoder (SqlT NonNull pgType) -> PGDecoder (SqlT Nullable pgType)
nullifyPG DecodePG = DecodePG

decoderToColumn :: PGDecoder t -> Dec.Row (HaskellT t)
decoderToColumn (DecodePG @n @pgt) =
  case nullabilityS @n of
    NonNullSing -> HaskT <$> Dec.column (Dec.nonNullable $ decodeHaskellValue (Proxy @pgt))
    NullableSing -> HaskT <$> Dec.column (Dec.nullable $ decodeHaskellValue (Proxy @pgt))

useRowDecoder :: (FTraversable t) => t PGDecoder -> Dec.Row (t HaskellT)
useRowDecoder = ftraverse decoderToColumn

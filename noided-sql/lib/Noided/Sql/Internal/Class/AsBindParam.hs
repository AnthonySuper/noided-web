{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Class.AsBindParam where

import Data.Functor.Contravariant
import Data.Int
import Data.Kind
import Data.Text (Text, pack)
import Data.Time
import Data.Typeable
import Data.UUID (UUID)
import Hasql.Encoders qualified as Enc
import Noided.Sql.Internal.Type.Nullability

type AsBindParam :: Type -> Constraint

type EncoderOf :: Nullability -> Type -> Type
data EncoderOf n t where
  EncodeNonNull :: Enc.Value t -> EncoderOf NonNull t
  EncodeNullable :: Enc.Value t -> EncoderOf Nullable (Maybe t)

-- | Type class representing values that can be used as bind parameters within a query.
class (Typeable t) => AsBindParam t where
  type BoundType t :: Type
  type BoundType t = t

  type BoundNullability t :: Nullability
  type BoundNullability _ = NonNull

  -- | Actual encoder for a bind param.
  bindParamEncoder :: EncoderOf (BoundNullability t) t

  -- | Inspect a bind param.
  -- This is never sent to Postgres, but can be used when rendering queries.
  inspectBindParam :: t -> Text
  default inspectBindParam :: (Show t) => t -> Text
  inspectBindParam = pack . show

  -- | Name used to *cast* a bind param, if available.
  -- This should be the postgres name of the type.
  -- Sometimes used for syntax generation.
  bindParamName :: proxy t -> Maybe Text
  bindParamName _ = Nothing

-- | We can bind nullable values.
instance (AsBindParam t, BoundNullability t ~ NonNull) => AsBindParam (Maybe t) where
  type BoundType (Maybe t) = t
  type BoundNullability (Maybe t) = Nullable
  bindParamEncoder =
    case bindParamEncoder @t of
      EncodeNonNull v -> EncodeNullable v
  inspectBindParam = maybe (pack "NULL") inspectBindParam
  bindParamName _ = bindParamName (Proxy @t)

instance AsBindParam Text where
  bindParamEncoder = EncodeNonNull Enc.text
  bindParamName _ = Just "text"

-- | Strings encode to the 'Text' type.
instance AsBindParam String where
  type BoundType String = Text
  bindParamEncoder = EncodeNonNull (contramap pack Enc.text)
  bindParamName _ = Just "text"

instance AsBindParam Bool where
  bindParamEncoder = EncodeNonNull Enc.bool
  bindParamName _ = Just "bool"

instance AsBindParam Int16 where
  bindParamEncoder = EncodeNonNull Enc.int2
  bindParamName _ = Just "int2"

instance AsBindParam Int32 where
  bindParamEncoder = EncodeNonNull Enc.int4
  bindParamName _ = Just "int4"

instance AsBindParam Int64 where
  bindParamEncoder = EncodeNonNull Enc.int8
  bindParamName _ = Just "int8"

instance AsBindParam Int where
  type BoundType Int = Int64
  bindParamEncoder = EncodeNonNull (contramap fromIntegral Enc.int8)
  bindParamName _ = Just "int8"

instance AsBindParam Float where
  bindParamEncoder = EncodeNonNull Enc.float4
  bindParamName _ = Just "float4"

instance AsBindParam Double where
  bindParamEncoder = EncodeNonNull Enc.float8
  bindParamName _ = Just "float8"

instance AsBindParam UUID where
  bindParamEncoder = EncodeNonNull Enc.uuid
  bindParamName _ = Just "uuid"

instance AsBindParam Day where
  bindParamEncoder = EncodeNonNull Enc.date
  bindParamName _ = Just "date"

instance AsBindParam TimeOfDay where
  bindParamEncoder = EncodeNonNull Enc.time
  bindParamName _ = Just "time"

instance AsBindParam UTCTime where
  bindParamEncoder = EncodeNonNull Enc.timestamptz
  bindParamName _ = Just "timestamptz"

instance AsBindParam LocalTime where
  bindParamEncoder = EncodeNonNull Enc.timestamp
  bindParamName _ = Just "timestamp"


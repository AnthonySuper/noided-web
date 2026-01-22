{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Class.AsBindParam where

import Data.Functor.Contravariant
import Data.Kind
import Data.Text (Text, pack)
import Data.Typeable
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

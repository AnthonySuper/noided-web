{-# LANGUAGE DefaultSignatures #-}

module Noided.Sql.Internal.Class.AsBindParam where

import Data.Kind
import Data.Text (Text, pack)
import Data.Typeable
import Hasql.Encoders qualified as Enc
import Noided.Sql.Internal.Type.SqlType

type AsBindParam :: Type -> Constraint

-- | Type class representing values that can be used
-- as bind parameters.
class (Typeable t) => AsBindParam t where
  type SqlTypeOf t :: SqlType
  type SqlTypeOf t = NonNullT t

  -- | Actual encoder for a bind param.
  bindParamEncoder :: Enc.NullableOrNot Enc.Value t

  -- | Inspect a bind param.
  -- This is never sent to Postgres, but can be used when rendering queries.
  inspectBindParam :: t -> Text
  default inspectBindParam :: (Show t) => t -> Text
  inspectBindParam = pack . show

  -- | Name used to *cast* a bind param, if available.
  -- This should be the postgres name of the type.
  -- Sometimes used for syntax generation.
  bindParamName :: proxy t -> Maybe Text

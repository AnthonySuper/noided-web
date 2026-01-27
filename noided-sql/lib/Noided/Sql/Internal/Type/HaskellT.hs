{-# LANGUAGE UndecidableInstances #-}

module Noided.Sql.Internal.Type.HaskellT where

import Data.Kind (Type)
import GHC.Generics
import Noided.Sql.Internal.Class.AsHaskellValue
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlType

type HaskellValueType :: SqlType -> Type
type family HaskellValueType sqlT where
  HaskellValueType (SqlT Nullable pgType) = Maybe (HaskellTypeOf pgType)
  HaskellValueType (SqlT NonNull pgType) = HaskellTypeOf pgType

newtype HaskellT wrapped = HaskT {getHaskT :: HaskellValueType wrapped}
  deriving (Generic)

deriving instance (Show (HaskellTypeOf t)) => Show (HaskellT (NonNullT t))

deriving instance (Eq (HaskellTypeOf t)) => Eq (HaskellT (NonNullT t))

deriving instance (Read (HaskellTypeOf t)) => Read (HaskellT (NonNullT t))

deriving instance (Ord (HaskellTypeOf t)) => Ord (HaskellT (NonNullT t))

deriving instance (Show (HaskellTypeOf t)) => Show (HaskellT (NullableT t))

deriving instance (Eq (HaskellTypeOf t)) => Eq (HaskellT (NullableT t))

deriving instance (Read (HaskellTypeOf t)) => Read (HaskellT (NullableT t))

deriving instance (Ord (HaskellTypeOf t)) => Ord (HaskellT (NullableT t))

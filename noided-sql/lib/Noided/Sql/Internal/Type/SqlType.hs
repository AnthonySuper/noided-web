module Noided.Sql.Internal.Type.SqlType where

import Data.Kind (Type)
import Noided.Sql.Internal.Type.Nullability

-- | Data Kind: used to represent a type in SQL, which has a particular nullability.
data SqlType = SqlT Nullability Type

type NullableT = SqlT Nullable

type NonNullT = SqlT NonNull

type SqlTypeNullability :: SqlType -> Nullability
type family SqlTypeNullability sqlT where
  SqlTypeNullability (SqlT n _) = n

type SqlTypePGType :: SqlType -> Type
type family SqlTypePGType sqlT where
  SqlTypePGType (SqlT _ pgT) = pgT

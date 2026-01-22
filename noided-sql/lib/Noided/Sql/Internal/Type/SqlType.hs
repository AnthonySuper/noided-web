module Noided.Sql.Internal.Type.SqlType where

import Data.Kind (Type)
import Noided.Sql.Internal.Type.Nullability

-- | Data Kind: used to represent a type in SQL, which has a particular nullability.
data SqlType = SqlT Nullability Type

type NullableT = SqlT Nullable

type NonNullT = SqlT NonNull

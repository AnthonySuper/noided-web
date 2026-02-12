module Noided.Sql.Internal.Type.ColumnType where

import Data.Kind (Type)
import GHC.Generics
import Noided.Sql.Internal.Class.AsHaskellValue
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlType

-- | If a column can be set to a DEFAULT value or not.
data ColumnDefault
  = NoDefault
  | MayBeDefault
  | AlwaysDefault
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

data ColumnType
  = Column ColumnDefault Nullability Type
  deriving (Generic)

type ColumnInQuery :: ColumnType -> SqlType
type family ColumnInQuery column where
  ColumnInQuery (Column _ n t) = SqlT n t

type ColumnNullifiedInQuery :: ColumnType -> SqlType
type family ColumnNullifiedInQuery column where
  ColumnNullifiedInQuery (Column _ _ t) = SqlT Nullable t

type ColumnInHaskell :: ColumnType -> Type
type family ColumnInHaskell column where
  ColumnInHaskell (Column _ Nullable t) =
    Maybe (HaskellTypeOf t)
  ColumnInHaskell (Column _ NonNull t) =
    HaskellTypeOf t

type IdentityColumn = Column AlwaysDefault NonNull

type RegularColumn = Column NoDefault NonNull

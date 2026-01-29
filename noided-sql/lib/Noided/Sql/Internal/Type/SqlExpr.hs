module Noided.Sql.Internal.Type.SqlExpr where

import Data.Coerce
import Data.Kind (Constraint, Type)
import GHC.Generics
import GHC.TypeLits
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax

data SqlScope
  = -- | Normal query values.
    NormalQuery
  | -- | Query values with window functions.
    Windowed
  | -- | Query values that are in an aggregate set (and must be used with an aggregate function).
    AggregateSet
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

-- | An SQL expression with some given type.
--
-- This is an opaque representation of the SQL syntax behind the expression.
-- The types in Haskell keep you safe from using this improperly.
type SqlExpr :: SqlScope -> SqlType -> Type
newtype SqlExpr scope t = UnsafeMkSqlExpr {unsafeGetSqlExpr :: Syntax}

type QueriedRow t = t (SqlExpr NormalQuery)

type CastNullability :: Nullability -> Nullability -> Constraint
class CastNullability from to where
  castNullability :: SqlExpr scope (SqlT from a) -> SqlExpr scope (SqlT to a)

instance
  (TypeError (Text "Cannot cast a value that is " :<>: ShowType Nullable :<>: Text " to a value that is " :<>: ShowType NonNull)) =>
  CastNullability Nullable NonNull
  where
  castNullability = error "impossible: castNullability from nullable to nonNull"

instance CastNullability NonNull NonNull where
  castNullability = id

instance CastNullability Nullable Nullable where
  castNullability = id

instance CastNullability NonNull Nullable where
  castNullability = coerce

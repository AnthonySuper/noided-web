module Noided.Sql.Internal.Type.SqlExpr where

import Data.Kind (Type)
import GHC.Generics
import Noided.Sql.Internal.Type.SqlType (SqlType)
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

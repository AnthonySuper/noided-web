{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

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
  | -- | Query values that are aggregated (use aggregate functions)
    Aggregated
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

-- | An SQL expression with some given type.
--
-- This is an opaque representation of the SQL syntax behind the expression.
-- The types in Haskell keep you safe from using this improperly.
type SqlExpr :: SqlScope -> SqlType -> Type
data SqlExpr scope t = MkSqlExpr
  { sqlExprShape :: !ExprShape,
    unsafeGetSqlExpr :: Syntax
  }

-- | Whether an expression is syntactically self-delimiting.
--
-- Only 'Atom' expressions (columns, bind parameters, literals, function calls,
-- subqueries, @CASE ... END@) may be embedded in an operator expression without
-- parentheses. Everything else is 'Compound' and is wrapped by 'operand'.
-- There is deliberately no precedence table: when in doubt, use 'Compound'.
data ExprShape = Atom | Compound
  deriving (Show, Eq)

-- | Build an expression that is /not/ known to be an atom (the conservative default).
pattern UnsafeMkSqlExpr :: Syntax -> SqlExpr scope t
pattern UnsafeMkSqlExpr syn <- MkSqlExpr _ syn
  where
    UnsafeMkSqlExpr syn = MkSqlExpr Compound syn

{-# COMPLETE UnsafeMkSqlExpr #-}

-- | Build an expression that is a syntactic atom: it can be safely embedded in
-- any operator expression without parentheses. Only use this for column
-- references, bind parameters, literals, function calls, subqueries, and other
-- self-delimiting forms.
unsafeMkAtom :: Syntax -> SqlExpr scope t
unsafeMkAtom = MkSqlExpr Atom

-- | Render an expression as an operand of an operator: atoms are written as is, anything else is parenthesized.
operand :: SqlExpr scope t -> Syntax
operand (MkSqlExpr Atom syn) = syn
operand (MkSqlExpr Compound syn) = "(" <> syn <> ")"

-- | Change the type of an expression, keeping its syntax (and shape).
unsafeRetypeSqlExpr :: SqlExpr scope t -> SqlExpr scope t'
unsafeRetypeSqlExpr (MkSqlExpr sh syn) = MkSqlExpr sh syn

type QueriedRow t = t (SqlExpr NormalQuery)

type QueriedExpr = SqlExpr NormalQuery

type AggregateSetRow t = t (SqlExpr AggregateSet)

type AggregateSetExpr = SqlExpr AggregateSet

type AggregatedRow t = t (SqlExpr Aggregated)

type AggregatedExpr = SqlExpr Aggregated

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

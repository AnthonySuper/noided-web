{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.Bool where

import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.QueryWriter (syntaxSubquery)
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType

infix 4 ==., <., >., <=., >=., /=.

infixr 3 &&.

infixr 2 ||.

(==.),
  (<.),
  (>.),
  (<=.),
  (>=.),
  (/=.) ::
    SqlExpr scope (SqlT lhsN a) ->
    SqlExpr scope (SqlT rhsN a) ->
    SqlExpr scope (SqlT (MostNullable lhsN rhsN) Bool)
(==.) a b = UnsafeMkSqlExpr (operand a <> " = " <> operand b)
(<.) a b = UnsafeMkSqlExpr (operand a <> " < " <> operand b)
(>.) a b = UnsafeMkSqlExpr (operand a <> " > " <> operand b)
(<=.) a b = UnsafeMkSqlExpr (operand a <> " <= " <> operand b)
(>=.) a b = UnsafeMkSqlExpr (operand a <> " >= " <> operand b)
(/=.) a b = UnsafeMkSqlExpr (operand a <> " <> " <> operand b)

isNull_ :: SqlExpr scope (SqlT Nullable a) -> SqlExpr scope (NonNullT Bool)
isNull_ a = UnsafeMkSqlExpr (operand a <> " IS NULL")

isNotNull_ :: SqlExpr scope (SqlT Nullable a) -> SqlExpr scope (NonNullT Bool)
isNotNull_ a = UnsafeMkSqlExpr (operand a <> " IS NOT NULL")

(&&.),
  (||.) ::
    SqlExpr scope (SqlT lhsN Bool) ->
    SqlExpr scope (SqlT rhsN Bool) ->
    SqlExpr scope (SqlT (MostNullable lhsN rhsN) Bool)
(&&.) a b = UnsafeMkSqlExpr (operand a <> " AND " <> operand b)
(||.) a b = UnsafeMkSqlExpr (operand a <> " OR " <> operand b)

coalesce_ :: SqlExpr scope (SqlT lhsN t) -> SqlExpr scope (SqlT rhsN t) -> SqlExpr scope (SqlT (LeastNullable lhsN rhsN) t)
coalesce_ a b =
  UnsafeMkSqlExpr $
    "COALESCE("
      <> unsafeGetSqlExpr a
      <> ","
      <> unsafeGetSqlExpr b
      <> ")"

true_, false_ :: SqlExpr scope (NonNullT Bool)
true_ = unsafeMkAtom "TRUE"
false_ = unsafeMkAtom "FALSE"

not_ :: SqlExpr scope (SqlT n Bool) -> SqlExpr scope (SqlT n Bool)
not_ a = UnsafeMkSqlExpr ("NOT " <> operand a)

-- | @EXISTS (subquery)@: true when the given query returns at least one row.
--
-- The query is rendered as a subquery, so it may be any 'SelectQuery'.
exists_ :: (SelectQuery query) => query -> SqlExpr scope (NonNullT Bool)
exists_ query =
  unsafeMkAtom $
    "EXISTS (" <> syntaxSubquery (writeQuerySyntax query) <> ")"

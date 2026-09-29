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
(==.) a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") = (" <> unsafeGetSqlExpr b <> ")")
(<.) a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") < (" <> unsafeGetSqlExpr b <> ")")
(>.) a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") > (" <> unsafeGetSqlExpr b <> ")")
(<=.) a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") <= (" <> unsafeGetSqlExpr b <> ")")
(>=.) a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") >= (" <> unsafeGetSqlExpr b <> ")")
(/=.) a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") <> (" <> unsafeGetSqlExpr b <> ")")

isNull_ :: SqlExpr scope (SqlT Nullable a) -> SqlExpr scope (NonNullT Bool)
isNull_ a = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") IS NULL")

isNotNull_ :: SqlExpr scope (SqlT Nullable a) -> SqlExpr scope (NonNullT Bool)
isNotNull_ a = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") IS NOT NULL")

(&&.),
  (||.) ::
    SqlExpr scope (SqlT lhsN Bool) ->
    SqlExpr scope (SqlT rhsN Bool) ->
    SqlExpr scope (SqlT (MostNullable lhsN rhsN) Bool)
(&&.) a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") AND (" <> unsafeGetSqlExpr b <> ")")
(||.) a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") OR (" <> unsafeGetSqlExpr b <> ")")

coalesce_ :: SqlExpr scope (SqlT lhsN t) -> SqlExpr scope (SqlT rhsN t) -> SqlExpr scope (SqlT (LeastNullable lhsN rhsN) t)
coalesce_ a b =
  UnsafeMkSqlExpr $
    "COALESCE("
      <> unsafeGetSqlExpr a
      <> ","
      <> unsafeGetSqlExpr b
      <> ")"

true_, false_ :: SqlExpr scope (NonNullT Bool)
true_ = UnsafeMkSqlExpr "TRUE"
false_ = UnsafeMkSqlExpr "FALSE"

not_ :: SqlExpr scope (SqlT n Bool) -> SqlExpr scope (SqlT n Bool)
not_ a = UnsafeMkSqlExpr ("NOT (" <> unsafeGetSqlExpr a <> ")")

exists_ :: (SelectQuery query) => query -> SqlExpr scope (NonNullT Bool)
exists_ query =
  UnsafeMkSqlExpr $
    "EXISTS (" <> syntaxSubquery (writeQuerySyntax query) <> ")"

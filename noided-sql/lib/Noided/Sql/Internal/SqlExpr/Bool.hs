{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.Bool where

import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType

infix 4 ==.

infixr 3 &&.

infixr 2 ||.

(==.) ::
  SqlExpr scope (SqlT lhsN a) ->
  SqlExpr scope (SqlT rhsN a) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) Bool)
(==.) a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") = (" <> unsafeGetSqlExpr b <> ")")

(&&.),
  (||.) ::
    SqlExpr scope (SqlT lhsN Bool) ->
    SqlExpr scope (SqlT rhsN Bool) ->
    SqlExpr scope (SqlT (MostNullable lhsN rhsN) Bool)
(&&.) a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") AND (" <> unsafeGetSqlExpr b <> ")")
(||.) a b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") OR (" <> unsafeGetSqlExpr b <> ")")

true_, false_ :: SqlExpr scope (NonNullT Bool)
true_ = UnsafeMkSqlExpr "TRUE"
false_ = UnsafeMkSqlExpr "FALSE"

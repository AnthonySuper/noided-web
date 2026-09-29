{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.Numeric where

import Noided.Sql.Internal.Class.SqlNumeric
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType

infixl 6 +., -.
infixl 7 *., /.

(+.), (-.), (*.), (/.) ::
  (SqlNumeric a) =>
  SqlExpr scope (SqlT lhsN a) ->
  SqlExpr scope (SqlT rhsN a) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) a)

a +. b = UnsafeMkSqlExpr (operand a <> " + " <> operand b)
a -. b = UnsafeMkSqlExpr (operand a <> " - " <> operand b)
a *. b = UnsafeMkSqlExpr (operand a <> " * " <> operand b)
a /. b = UnsafeMkSqlExpr (operand a <> " / " <> operand b)

abs_ :: (SqlNumeric a) => SqlExpr scope (SqlT n a) -> SqlExpr scope (SqlT n a)
abs_ a = unsafeMkAtom ("ABS(" <> unsafeGetSqlExpr a <> ")")

negate_ :: (SqlNumeric a) => SqlExpr scope (SqlT n a) -> SqlExpr scope (SqlT n a)
negate_ a = UnsafeMkSqlExpr ("-" <> operand a)

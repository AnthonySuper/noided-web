{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.Text where

import Data.Int (Int32)
import Data.Text (Text)
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType

infixl 6 <>.

-- | Concatenation operator @||@.
(<>.) ::
  SqlExpr scope (SqlT lhsN Text) ->
  SqlExpr scope (SqlT rhsN Text) ->
  SqlExpr scope (SqlT (MostNullable lhsN rhsN) Text)
a <>. b = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr a <> ") || (" <> unsafeGetSqlExpr b <> ")")

-- | Sql @LOWER@ function.
lower_ :: SqlExpr scope (SqlT n Text) -> SqlExpr scope (SqlT n Text)
lower_ a = UnsafeMkSqlExpr ("LOWER(" <> unsafeGetSqlExpr a <> ")")

-- | Sql @UPPER@ function.
upper_ :: SqlExpr scope (SqlT n Text) -> SqlExpr scope (SqlT n Text)
upper_ a = UnsafeMkSqlExpr ("UPPER(" <> unsafeGetSqlExpr a <> ")")

-- | Sql @LENGTH@ function.
length_ :: SqlExpr scope (SqlT n Text) -> SqlExpr scope (SqlT n Int32)
length_ a = UnsafeMkSqlExpr ("LENGTH(" <> unsafeGetSqlExpr a <> ")")

-- | Sql @TRIM@ function.
trim_ :: SqlExpr scope (SqlT n Text) -> SqlExpr scope (SqlT n Text)
trim_ a = UnsafeMkSqlExpr ("TRIM(" <> unsafeGetSqlExpr a <> ")")

-- | Sql @LTRIM@ function.
ltrim_ :: SqlExpr scope (SqlT n Text) -> SqlExpr scope (SqlT n Text)
ltrim_ a = UnsafeMkSqlExpr ("LTRIM(" <> unsafeGetSqlExpr a <> ")")

-- | Sql @RTRIM@ function.
rtrim_ :: SqlExpr scope (SqlT n Text) -> SqlExpr scope (SqlT n Text)
rtrim_ a = UnsafeMkSqlExpr ("RTRIM(" <> unsafeGetSqlExpr a <> ")")

-- | Sql @SUBSTR@ function.
substr_ ::
  SqlExpr scope (SqlT n Text) ->
  SqlExpr scope (SqlT n' Int32) ->
  SqlExpr scope (SqlT n'' Int32) ->
  SqlExpr scope (SqlT (MostNullable n (MostNullable n' n'')) Text)
substr_ s f c =
  UnsafeMkSqlExpr ("SUBSTR(" <> unsafeGetSqlExpr s <> ", " <> unsafeGetSqlExpr f <> ", " <> unsafeGetSqlExpr c <> ")")

-- | Sql @REPLACE@ function.
replace_ ::
  SqlExpr scope (SqlT n Text) ->
  SqlExpr scope (SqlT n' Text) ->
  SqlExpr scope (SqlT n'' Text) ->
  SqlExpr scope (SqlT (MostNullable n (MostNullable n' n'')) Text)
replace_ s f t =
  UnsafeMkSqlExpr ("REPLACE(" <> unsafeGetSqlExpr s <> ", " <> unsafeGetSqlExpr f <> ", " <> unsafeGetSqlExpr t <> ")")

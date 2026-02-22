{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.Range where

import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import PostgreSQL.Binary.Range (Range)

-- | Range contains element operator (@ @> @)
contains_ ::
  SqlExpr scope (SqlT n1 (Range a)) ->
  SqlExpr scope (SqlT n2 a) ->
  SqlExpr scope (SqlT (MostNullable n1 n2) Bool)
contains_ r e = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr r <> ") @> (" <> unsafeGetSqlExpr e <> ")")

-- | Element is contained by range operator (@ <@ @)
isContainedBy_ ::
  SqlExpr scope (SqlT n1 a) ->
  SqlExpr scope (SqlT n2 (Range a)) ->
  SqlExpr scope (SqlT (MostNullable n1 n2) Bool)
isContainedBy_ e r = UnsafeMkSqlExpr ("(" <> unsafeGetSqlExpr e <> ") <@ (" <> unsafeGetSqlExpr r <> ")")

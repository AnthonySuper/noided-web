{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.Row where

import Data.HKD
import Noided.Sql.Internal.Type.PGRow
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax

row_ :: (FFoldable rowLike) => rowLike (SqlExpr scope) -> SqlExpr scope (NonNullT (PGRow rowLike))
row_ expr =
  UnsafeMkSqlExpr $
    "ROW("
      <> fromCommaSepSyntax (ffoldMap toExpr expr)
      <> ")"
  where
    toExpr :: SqlExpr anything anything' -> CommaSepSyntax
    toExpr = Written . unsafeGetSqlExpr

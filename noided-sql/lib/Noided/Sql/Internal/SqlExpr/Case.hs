{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.Case where

import Noided.Sql.Internal.Type.CaseBranch
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType

case_ ::
  [CaseBranch scope (SqlT n Bool) evaluated] ->
  SqlExpr scope evaluated ->
  SqlExpr scope evaluated
case_ branches elseClause =
  UnsafeMkSqlExpr $
    "CASE "
      <> foldMap unsafeRenderCaseBranch branches
      <> " ELSE "
      <> unsafeGetSqlExpr elseClause
      <> " END"

caseNoElse_ ::
  [CaseBranch scope (SqlT n Bool) (SqlT anyN res)] ->
  SqlExpr scope (NullableT res)

caseNoElse branches =
  UnsafeMkSqlExpr $
    "CASE "
      <> foldMap unsafeRenderCaseBranch branches

caseSimple_ ::
  SqlExpr scope compared ->
  [CaseBranch scope compared evaluated] ->
  SqlExpr scope evaluated ->
  SqlExpr scope evaluated
caseSimple_ comp branches elseClause =
  UnsafeMkSqlExpr $
    "CASE "
      <> unsafeGetSqlExpr comp
      <> " "
      <> foldMap unsafeRenderCaseBranch branches
      <> " ELSE "
      <> unsafeGetSqlExpr elseClause
      <> " END"

{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.SqlExpr.Case
  ( case_,
    caseNoElse_,
    caseSimple_,
  )
where

import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Noided.Sql.Internal.Type.CaseBranch
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType

-- | Standard CASE expression (at least one branch is required): CASE WHEN cond1 THEN val1 WHEN cond2 THEN val2 ELSE elseVal END
case_ ::
  NonEmpty (CaseBranch scope (SqlT n Bool) evaluated) ->
  SqlExpr scope evaluated ->
  SqlExpr scope evaluated
case_ branches elseClause =
  unsafeMkAtom $
    "CASE "
      <> mconcat (intersperse " " (map unsafeRenderCaseBranch (NE.toList branches)))
      <> " ELSE "
      <> unsafeGetSqlExpr elseClause
      <> " END"

-- | CASE expression without ELSE: CASE WHEN cond1 THEN val1 END
-- Evaluates to NULL if no condition is met.
caseNoElse_ ::
  NonEmpty (CaseBranch scope (SqlT n Bool) (SqlT anyN res)) ->
  SqlExpr scope (NullableT res)
caseNoElse_ branches =
  unsafeMkAtom $
    "CASE "
      <> mconcat (intersperse " " (map unsafeRenderCaseBranch (NE.toList branches)))
      <> " END"

-- | Simple CASE expression: CASE comp WHEN val1 THEN res1 ELSE elseVal END
caseSimple_ ::
  SqlExpr scope compared ->
  NonEmpty (CaseBranch scope compared evaluated) ->
  SqlExpr scope evaluated ->
  SqlExpr scope evaluated
caseSimple_ comp branches elseClause =
  unsafeMkAtom $
    "CASE "
      <> unsafeGetSqlExpr comp
      <> " "
      <> mconcat (intersperse " " (map unsafeRenderCaseBranch (NE.toList branches)))
      <> " ELSE "
      <> unsafeGetSqlExpr elseClause
      <> " END"


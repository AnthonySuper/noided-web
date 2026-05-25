{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Type.CaseBranch where

import Data.Kind (Type)
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax

type CaseBranch :: SqlScope -> SqlType -> SqlType -> Type
data CaseBranch scope compared evaluated where
  MkCaseBranch ::
    SqlExpr scope compared ->
    SqlExpr scope evaluated ->
    CaseBranch scope compared evaluated

infix 5 `then_`

then_ :: SqlExpr scope compared -> SqlExpr scope evaluated -> CaseBranch scope compared evaluated
then_ = MkCaseBranch

unsafeRenderCaseBranch :: CaseBranch scope t1 t2 -> Syntax
unsafeRenderCaseBranch (MkCaseBranch compared evaluated) =
  "WHEN "
    <> unsafeGetSqlExpr compared
    <> " THEN "
    <> unsafeGetSqlExpr evaluated

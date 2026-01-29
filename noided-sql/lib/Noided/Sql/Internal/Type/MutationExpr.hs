{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Type.MutationExpr where

import Data.Kind
import Noided.Sql.Internal.Type.MutationType
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.Syntax (Syntax)

type MutationExpr :: MutationType -> Type
data MutationExpr val where
  MutateVal :: SqlExpr NormalQuery t -> MutationExpr (ActualValue t)
  DefaultVal :: MutationExpr DefaultValue

unsafeMutationExprToSyntax :: MutationExpr val -> Syntax
unsafeMutationExprToSyntax = \case
  MutateVal (UnsafeMkSqlExpr e) -> e
  DefaultVal -> "DEFAULT"

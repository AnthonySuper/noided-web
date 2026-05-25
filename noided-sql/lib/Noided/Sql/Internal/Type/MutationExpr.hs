{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Type.MutationExpr where

import Data.Kind
import Noided.Sql.Internal.SqlExpr.Bind
import Noided.Sql.Internal.Type.MutationType
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.Syntax (Syntax)

type MutationExpr :: MutationType -> Type
data MutationExpr val where
  MutateVal :: SqlExpr NormalQuery t -> MutationExpr (ActualValue t)
  DefaultVal :: MutationExpr DefaultValue

mutateVal_ :: SqlExpr NormalQuery t -> MutationExpr (ActualValue t)
mutateVal_ = MutateVal

defaultVal_ :: MutationExpr DefaultValue
defaultVal_ = DefaultVal

unsafeMutationExprToSyntax :: MutationExpr val -> Syntax
unsafeMutationExprToSyntax = \case
  MutateVal (UnsafeMkSqlExpr e) -> e
  DefaultVal -> "DEFAULT"

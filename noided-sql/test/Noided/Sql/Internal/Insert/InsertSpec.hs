{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Noided.Sql.Internal.Insert.InsertSpec (spec) where

import Data.Text (unpack, Text)
import Data.Int (Int64)
import Data.Coerce (coerce)
import Noided.Row
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Insert.Insert
import Noided.Sql.Internal.Insert.InsertValues
import Noided.Sql.Internal.Select.SelectM
import Noided.Sql.Internal.SqlExpr.Bind
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.ColumnType
import Noided.Sql.Internal.Type.MutationExpr
import Noided.Sql.Internal.Type.MutationType
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax
import Noided.Sql.Internal.Type.TableDefinition
import Test.Hspec
import Test.Hspec.Golden
import qualified Data.List.NonEmpty as NE

type UserTable =
  '[ "id" :=> IdentityColumn Int64,
     "name" :=> RegularColumn Text,
     "email" :=> Column MayBeDefault Nullable Text
   ]

userTable :: TableDefinition UserTable (WrappedRow (RowLabelsInQuery UserTable))
userTable = DefineTable "users" cols (coerce cols)
  where
    cols =
      MkColumnName "id"
      :::% MkColumnName "name"
      :::% MkColumnName "email"
      :::% EmptyWrappedRow

type TableWithDefaults =
  '[ "id" :=> IdentityColumn Int64,
     "created_at" :=> Column AlwaysDefault NonNull Text
   ]

tableWithDefaults :: TableDefinition TableWithDefaults (WrappedRow (RowLabelsInQuery TableWithDefaults))
tableWithDefaults = DefineTable "defaults_table" cols (coerce cols)
  where
    cols = MkColumnName "id" :::% MkColumnName "created_at" :::% EmptyWrappedRow

type FullInsertRow =
  '[ "id" :=> DefaultValue,
     "name" :=> ActualValue (SqlT NonNull Text),
     "email" :=> ActualValue (SqlT NonNull Text)
   ]

type PartialInsertRow =
  '[ "name" :=> ActualValue (SqlT NonNull Text) ]

type SelectedRow =
  '[ "name" :=> SqlT NonNull Text,
     "email" :=> SqlT NonNull Text
   ]

renderGolden ::
  (Query q) =>
  String ->
  q ->
  Spec
renderGolden description query =
  golden description (return syntaxString)
  where
    syntaxString = unpack (renderSyntaxToTextNumberedBinds syntax)
    syntax = renderQueryWriter (writeQuerySyntax query)

spec :: Spec
spec = describe "InsertQuery" $ do
  renderGolden "insert-default-values" $
    insertDefaultValuesReturning tableWithDefaults id

  renderGolden "insert-single-row" $
    insertReturningAll userTable $
      (ValuesList $
        NE.fromList
          [ DefaultVal
            :::% MutateVal (bindParam @Text "Alice")
            :::% MutateVal (bindParam @Text "alice@example.com")
            :::% EmptyWrappedRow
          ] :: InsertValues FullInsertRow)
  
  renderGolden "insert-multiple-rows" $
    insertReturningAll userTable $
      (ValuesList $
        NE.fromList
          [ DefaultVal
            :::% MutateVal (bindParam @Text "Alice")
            :::% MutateVal (bindParam @Text "alice@example.com")
            :::% EmptyWrappedRow
          , DefaultVal
            :::% MutateVal (bindParam @Text "Bob")
            :::% MutateVal (bindParam @Text "bob@example.com")
            :::% EmptyWrappedRow
          ] :: InsertValues FullInsertRow)

  renderGolden "insert-partial-columns" $
    insertReturningAll userTable $
       (ValuesList $
         NE.fromList
           [ MutateVal (bindParam @Text "Charlie")
             :::% EmptyWrappedRow
           ] :: InsertValues PartialInsertRow)

  renderGolden "insert-select" $
    insertReturningAll userTable $
      InsertSelect (do
        return (
          bindParam @Text "Dave"
          :::% bindParam @Text "dave@example.com"
          :::% EmptyWrappedRow
          ) :: SelectM (WrappedRow SelectedRow (SqlExpr NormalQuery)))

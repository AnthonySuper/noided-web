{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Noided.Sql.Internal.Update.UpdateSpec (spec) where

import Data.Coerce
import Data.Int (Int64)
import Data.Text (unpack, Text)
import Noided.Row
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Select.FromClause
import Noided.Sql.Internal.Select.SelectM
import Noided.Sql.Internal.SqlExpr.Bind
import Noided.Sql.Internal.SqlExpr.Bool ((==.))
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.ColumnType
import Noided.Sql.Internal.Type.MutationExpr
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax
import Noided.Sql.Internal.Type.TableDefinition
import Noided.Sql.Internal.Type.TableName
import Noided.Sql.Internal.Update.Sets
import Noided.Sql.Internal.Update.Update
import Test.Hspec
import Test.Hspec.Golden

-- Define Table
type UserTable =
  '[ "id" :=> IdentityColumn Int64,
     "name" :=> RegularColumn Text,
     "email" :=> Column MayBeDefault Nullable Text,
     "score" :=> RegularColumn Int64
   ]

userTable :: TableDefinition UserTable (WrappedRow (RowLabelsInQuery UserTable))
userTable = DefineTable "users" cols (coerce cols)
  where
    cols =
      MkColumnName "id"
      :::% MkColumnName "name"
      :::% MkColumnName "email"
      :::% MkColumnName "score"
      :::% EmptyWrappedRow

type IdRow = '[ "id" :=> SqlT NonNull Int64 ]

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
spec = describe "UpdateQuery" $ do
  renderGolden "simple-update" $
    updateReturning userTable $ \r -> do
      return (#name |= MutateVal (bindParam @Text "New Name"), r)

  renderGolden "update-with-where" $
    updateReturning userTable $ \r -> do
      addWhere_ (r.id ==. bindParam @Int64 123)
      return (#score |= MutateVal (bindParam @Int64 100), r.id :::% EmptyWrappedRow) :: SelectM (ColumnUpdates UserTable, WrappedRow IdRow (SqlExpr NormalQuery))

  renderGolden "update-from-another-table" $
    updateReturning userTable $ \u -> do
      -- Join with another table (self-join for simplicity of test definition)
      other <- addFrom_ (fromBase_ (mkTableDef "other_users"))
      
      addWhere_ (u.id ==. other.id)
      
      return (#score |= MutateVal other.score, u)

mkTableDef :: Text -> TableDefinition UserTable (WrappedRow (RowLabelsInQuery UserTable))
mkTableDef name = DefineTable (tableNameNoSchema name) cols (coerce cols)
  where
    cols =
      MkColumnName "id"
      :::% MkColumnName "name"
      :::% MkColumnName "email"
      :::% MkColumnName "score"
      :::% EmptyWrappedRow
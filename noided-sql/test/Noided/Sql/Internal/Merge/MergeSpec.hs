{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Noided.Sql.Internal.Merge.MergeSpec (spec) where

import Data.Coerce
import Data.Int (Int64)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text, unpack)
import Noided.Row
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Insert.InsertValues
import Noided.Sql.Internal.Merge.Merge
import Noided.Sql.Internal.SqlExpr.Bind
import Noided.Sql.Internal.SqlExpr.Bool ((==.), (>.))
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
import Noided.Sql.Internal.Type.TableName
import Noided.Sql.Internal.Update.Sets
import Test.Hspec
import Test.Hspec.Golden

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

mkSourceTable :: Text -> TableDefinition UserTable (WrappedRow (RowLabelsInQuery UserTable))
mkSourceTable name = DefineTable (tableNameNoSchema name) cols (coerce cols)
  where
    cols =
      MkColumnName "id"
        :::% MkColumnName "name"
        :::% MkColumnName "email"
        :::% MkColumnName "score"
        :::% EmptyWrappedRow

type NameEmailScoreInsertRow =
  '[ "name" :=> ActualValue (SqlT NonNull Text),
     "email" :=> ActualValue (SqlT Nullable Text),
     "score" :=> ActualValue (SqlT NonNull Int64)
   ]

type IdRow = '["id" :=> SqlT NonNull Int64]

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
spec = describe "MergeQuery" $ do
  renderGolden "merge-matched-update-not-matched-insert" $
    mergeReturningAll
      userTable
      (mkSourceTable "source_users")
      (\t s -> t.id ==. s.id)
      ( whenMatched_ (MergeUpdate $ \_ s -> #name |= MutateVal s.name)
          NE.:| [ whenNotMatched_ $ MergeInsert $ \_ s ->
                    ( ValuesList $
                        NE.fromList
                          [ MutateVal s.name
                              :::% MutateVal s.email
                              :::% MutateVal s.score
                              :::% EmptyWrappedRow
                          ] ::
                        InsertValues NameEmailScoreInsertRow
                    )
                ]
      )

  renderGolden "merge-matched-delete" $
    mergeReturningAll
      userTable
      (mkSourceTable "source_users")
      (\t s -> t.id ==. s.id)
      (NE.singleton $ whenMatched_ MergeDelete)

  renderGolden "merge-do-nothing" $
    mergeReturning
      userTable
      (mkSourceTable "source_users")
      (\t s -> t.id ==. s.id)
      (NE.singleton $ whenMatched_ MergeDoNothing)
      (\r -> r.id :::% EmptyWrappedRow :: WrappedRow IdRow (SqlExpr NormalQuery))

  renderGolden "merge-with-extra-condition" $
    mergeReturning
      userTable
      (mkSourceTable "source_users")
      (\t s -> t.id ==. s.id)
      ( andMergeCondition_ (\t _ -> t.score >. bindParam @Int64 100) (whenMatched_ MergeDelete)
          NE.:| [whenNotMatchedBySource_ MergeDelete]
      )
      (\r -> r.id :::% EmptyWrappedRow :: WrappedRow IdRow (SqlExpr NormalQuery))

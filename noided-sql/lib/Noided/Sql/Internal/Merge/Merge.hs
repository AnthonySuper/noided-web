{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Noided.Sql.Internal.Merge.Merge where

import Data.Foldable (for_)
import Noided.Row
import Noided.Sql.Internal.Class.DecodeSelectList
import Noided.Sql.Internal.Class.FromItem
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Class.SelectList
import Noided.Sql.Internal.Class.UnwrapSelectList
import Noided.Sql.Internal.Insert.InsertValues
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.TableDefinition
import Noided.Sql.Internal.Type.TableName
import Noided.Sql.Internal.Update.Sets

-- | Specifies when a MERGE clause should apply.
data MergeWhenCondition
  = WhenMatched
  | WhenNotMatched
  | WhenNotMatchedBySource

-- | The action to take when a MERGE clause fires.
data MergeClauseAction targetCols targetSelectList sourceSelectList where
  -- | Do nothing when the clause matches.
  MergeDoNothing :: MergeClauseAction targetCols targetSelectList sourceSelectList
  -- | Delete the target row when the clause matches.
  MergeDelete :: MergeClauseAction targetCols targetSelectList sourceSelectList
  -- | Update columns of the target row when the clause matches.
  MergeUpdate ::
    (QueriedRow targetSelectList -> QueriedRow sourceSelectList -> ColumnUpdates targetCols) ->
    MergeClauseAction targetCols targetSelectList sourceSelectList
  -- | Insert a new row when the clause matches.
  MergeInsert ::
    (InsertForTable targetCols insertedCols) =>
    (QueriedRow targetSelectList -> QueriedRow sourceSelectList -> InsertValues insertedCols) ->
    MergeClauseAction targetCols targetSelectList sourceSelectList

-- | A single WHEN clause in a MERGE statement.
data MergeClause targetCols targetSelectList sourceSelectList where
  MergeWhen ::
    forall targetCols targetSelectList sourceSelectList n.
    MergeWhenCondition ->
    Maybe (QueriedRow targetSelectList -> QueriedRow sourceSelectList -> SqlExpr NormalQuery (SqlT n Bool)) ->
    MergeClauseAction targetCols targetSelectList sourceSelectList ->
    MergeClause targetCols targetSelectList sourceSelectList

-- | A MERGE query with a RETURNING clause.
data MergeQuery returning where
  Merge ::
    forall targetCols targetSelectList source n returning.
    (SelectList targetSelectList, FromItem source) =>
    TableDefinition targetCols targetSelectList ->
    source ->
    (QueriedRow targetSelectList -> QueriedRow (FromItemSelectList source) -> SqlExpr NormalQuery (SqlT n Bool)) ->
    [MergeClause targetCols targetSelectList (FromItemSelectList source)] ->
    (QueriedRow targetSelectList -> returning) ->
    MergeQuery returning

instance Functor MergeQuery where
  fmap f (Merge td src onCond clauses br) = Merge td src onCond clauses (f . br)

-- | Construct a MERGE query with a custom RETURNING clause.
mergeReturning ::
  (SelectList targetSelectList, FromItem source) =>
  TableDefinition targetCols targetSelectList ->
  source ->
  (QueriedRow targetSelectList -> QueriedRow (FromItemSelectList source) -> SqlExpr NormalQuery (SqlT n Bool)) ->
  [MergeClause targetCols targetSelectList (FromItemSelectList source)] ->
  (QueriedRow targetSelectList -> returning) ->
  MergeQuery returning
mergeReturning = Merge

-- | Construct a MERGE query returning all target table columns.
mergeReturningAll ::
  (SelectList targetSelectList, FromItem source) =>
  TableDefinition targetCols targetSelectList ->
  source ->
  (QueriedRow targetSelectList -> QueriedRow (FromItemSelectList source) -> SqlExpr NormalQuery (SqlT n Bool)) ->
  [MergeClause targetCols targetSelectList (FromItemSelectList source)] ->
  MergeQuery (QueriedRow targetSelectList)
mergeReturningAll td src onCond clauses = Merge td src onCond clauses id

-- | Construct a WHEN MATCHED THEN ... clause.
whenMatched_ ::
  MergeClauseAction targetCols targetSelectList sourceSelectList ->
  MergeClause targetCols targetSelectList sourceSelectList
whenMatched_ = MergeWhen WhenMatched Nothing

-- | Construct a WHEN NOT MATCHED THEN ... clause.
whenNotMatched_ ::
  MergeClauseAction targetCols targetSelectList sourceSelectList ->
  MergeClause targetCols targetSelectList sourceSelectList
whenNotMatched_ = MergeWhen WhenNotMatched Nothing

-- | Construct a WHEN NOT MATCHED BY SOURCE THEN ... clause.
whenNotMatchedBySource_ ::
  MergeClauseAction targetCols targetSelectList sourceSelectList ->
  MergeClause targetCols targetSelectList sourceSelectList
whenNotMatchedBySource_ = MergeWhen WhenNotMatchedBySource Nothing

-- | Add an extra AND condition to an existing merge clause.
andMergeCondition_ ::
  (QueriedRow targetSelectList -> QueriedRow sourceSelectList -> SqlExpr NormalQuery (SqlT n Bool)) ->
  MergeClause targetCols targetSelectList sourceSelectList ->
  MergeClause targetCols targetSelectList sourceSelectList
andMergeCondition_ cond (MergeWhen wc _ action) = MergeWhen wc (Just cond) action

writeMergeClause ::
  MergeClause targetCols targetSelectList sourceSelectList ->
  QueriedRow targetSelectList ->
  QueriedRow sourceSelectList ->
  WrappedRow targetCols ColumnName ->
  QueryWriter ()
writeMergeClause (MergeWhen cond extraCond action) targetRow sourceRow targetColNames = do
  case cond of
    WhenMatched -> " WHEN MATCHED"
    WhenNotMatched -> " WHEN NOT MATCHED"
    WhenNotMatchedBySource -> " WHEN NOT MATCHED BY SOURCE"
  for_ extraCond $ \f -> do
    " AND ("
    writeSyntax (unsafeGetSqlExpr (f targetRow sourceRow))
    ")"
  " THEN"
  case action of
    MergeDoNothing -> " DO NOTHING"
    MergeDelete -> " DELETE"
    MergeUpdate buildUpdates -> do
      " UPDATE SET "
      writeUpdateSets (buildUpdates targetRow sourceRow) targetColNames
    MergeInsert buildInsert -> do
      let iv = buildInsert targetRow sourceRow
      " INSERT"
      writeColumnListForInsert targetColNames iv
      " "
      writeInsertValues iv

writeMergeQuery ::
  (SelectList returningList) =>
  MergeQuery (QueriedRow returningList) ->
  QueryWriter ()
writeMergeQuery (Merge targetDef source onCond clauses buildReturning) = do
  "MERGE INTO "
  writeTableName targetDef.tableName
  " AS "
  targetAlias <- toUniqueAlias "to_merge"
  writeSyntax targetAlias
  let targetRow = qualifyColumnNames targetAlias targetDef.selectedNames
  " USING "
  writeFromItem source
  " AS "
  sourceRow <- writeFromItemAfterAs source
  " ON ("
  writeSyntax (unsafeGetSqlExpr (onCond targetRow sourceRow))
  ")"
  for_ clauses $ \clause ->
    writeMergeClause clause targetRow sourceRow targetDef.columnNames
  " RETURNING "
  writeSelectList (buildReturning targetRow)

instance
  (SelectList returningList) =>
  Query (MergeQuery (QueriedRow returningList))
  where
  type QuerySelectList (MergeQuery (QueriedRow returningList)) = returningList
  writeQuerySyntax = writeMergeQuery

instance
  (SelectList returningList, UnwrapSelectList returningList, DecodeSelectList returningList) =>
  ExecutableQuery (MergeQuery (QueriedRow returningList))

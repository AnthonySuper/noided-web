{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Noided.Sql.Internal.Merge.Merge where

import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty)
import Noided.Row
import Noided.Sql.Internal.Class.DecodeSelectList
import Noided.Sql.Internal.Class.FromItem
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Class.SelectList
import Noided.Sql.Internal.Class.UnwrapSelectList
import Noided.Sql.Internal.Insert.InsertValues
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.MutationExpr
import Noided.Sql.Internal.Type.MutationType
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.TableDefinition
import Noided.Sql.Internal.Type.TableName
import Noided.Sql.Internal.Update.Sets

-- | Specifies when a MERGE clause should apply.
-- Used as a data kind to index 'MergeClauseAction', so actions can only be used with a @WHEN@ kind Postgres allows.
data MergeWhenCondition
  = WhenMatched
  | WhenNotMatched
  | WhenNotMatchedBySource

-- | The values inserted by a MERGE.
-- Unlike a plain INSERT, MERGE only allows a single @VALUES@ row or @DEFAULT VALUES@.
data MergeInsertValues (insertedLabels :: [RowLabel MutationType]) where
  MergeDefaultValues :: MergeInsertValues '[]
  MergeSingleRow ::
    (labels ~ (x ': xs)) =>
    WrappedRow labels MutationExpr ->
    MergeInsertValues labels

-- | Insert a single row of values.
mergeValues_ ::
  (labels ~ (x ': xs)) =>
  WrappedRow labels MutationExpr ->
  MergeInsertValues labels
mergeValues_ = MergeSingleRow

-- | Insert using @DEFAULT VALUES@.
mergeDefaultValues_ :: MergeInsertValues '[]
mergeDefaultValues_ = MergeDefaultValues

toInsertValues :: MergeInsertValues labels -> InsertValues labels
toInsertValues MergeDefaultValues = DefaultValues
toInsertValues (MergeSingleRow r) = singleValue_ r

-- | The action to take when a MERGE clause fires.
-- The type index is the kind of @WHEN@ clause the action is valid for.
-- Each action only receives the rows that exist for that kind of clause:
-- there is no target row for @NOT MATCHED@ and no source row for @NOT MATCHED BY SOURCE@.
data MergeClauseAction (when :: MergeWhenCondition) targetCols targetSelectList sourceSelectList where
  -- | Do nothing when the clause matches. Valid for every kind of clause.
  MergeDoNothing :: MergeClauseAction when targetCols targetSelectList sourceSelectList
  -- | @WHEN MATCHED THEN DELETE@
  MergeMatchedDelete :: MergeClauseAction 'WhenMatched targetCols targetSelectList sourceSelectList
  -- | @WHEN MATCHED THEN UPDATE SET ...@
  MergeMatchedUpdate ::
    (QueriedRow targetSelectList -> QueriedRow sourceSelectList -> ColumnUpdates targetCols) ->
    MergeClauseAction 'WhenMatched targetCols targetSelectList sourceSelectList
  -- | @WHEN NOT MATCHED THEN INSERT ...@
  MergeNotMatchedInsert ::
    (InsertForTable targetCols insertedCols) =>
    (QueriedRow sourceSelectList -> MergeInsertValues insertedCols) ->
    MergeClauseAction 'WhenNotMatched targetCols targetSelectList sourceSelectList
  -- | @WHEN NOT MATCHED BY SOURCE THEN DELETE@ (PostgreSQL 17+)
  MergeBySourceDelete :: MergeClauseAction 'WhenNotMatchedBySource targetCols targetSelectList sourceSelectList
  -- | @WHEN NOT MATCHED BY SOURCE THEN UPDATE SET ...@ (PostgreSQL 17+)
  MergeBySourceUpdate ::
    (QueriedRow targetSelectList -> ColumnUpdates targetCols) ->
    MergeClauseAction 'WhenNotMatchedBySource targetCols targetSelectList sourceSelectList

-- | The condition function for a clause, which receives only the rows that exist for that kind of clause.
type family MergeConditionFn (when :: MergeWhenCondition) targetSelectList sourceSelectList result where
  MergeConditionFn 'WhenMatched t s r = QueriedRow t -> QueriedRow s -> r
  MergeConditionFn 'WhenNotMatched t s r = QueriedRow s -> r
  MergeConditionFn 'WhenNotMatchedBySource t s r = QueriedRow t -> r

-- | A single WHEN clause in a MERGE statement.
data MergeClause targetCols targetSelectList sourceSelectList where
  MergeWhenMatched ::
    forall targetCols targetSelectList sourceSelectList n.
    Maybe (MergeConditionFn 'WhenMatched targetSelectList sourceSelectList (SqlExpr NormalQuery (SqlT n Bool))) ->
    MergeClauseAction 'WhenMatched targetCols targetSelectList sourceSelectList ->
    MergeClause targetCols targetSelectList sourceSelectList
  MergeWhenNotMatched ::
    forall targetCols targetSelectList sourceSelectList n.
    Maybe (MergeConditionFn 'WhenNotMatched targetSelectList sourceSelectList (SqlExpr NormalQuery (SqlT n Bool))) ->
    MergeClauseAction 'WhenNotMatched targetCols targetSelectList sourceSelectList ->
    MergeClause targetCols targetSelectList sourceSelectList
  MergeWhenNotMatchedBySource ::
    forall targetCols targetSelectList sourceSelectList n.
    Maybe (MergeConditionFn 'WhenNotMatchedBySource targetSelectList sourceSelectList (SqlExpr NormalQuery (SqlT n Bool))) ->
    MergeClauseAction 'WhenNotMatchedBySource targetCols targetSelectList sourceSelectList ->
    MergeClause targetCols targetSelectList sourceSelectList

-- | A MERGE query with a RETURNING clause.
-- Note that @RETURNING@ requires PostgreSQL 17 or later, as does @WHEN NOT MATCHED BY SOURCE@.
data MergeQuery returning where
  Merge ::
    forall targetCols targetSelectList source n returning.
    (SelectList targetSelectList, FromItem source) =>
    TableDefinition targetCols targetSelectList ->
    source ->
    (QueriedRow targetSelectList -> QueriedRow (FromItemSelectList source) -> SqlExpr NormalQuery (SqlT n Bool)) ->
    NonEmpty (MergeClause targetCols targetSelectList (FromItemSelectList source)) ->
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
  NonEmpty (MergeClause targetCols targetSelectList (FromItemSelectList source)) ->
  (QueriedRow targetSelectList -> returning) ->
  MergeQuery returning
mergeReturning = Merge

-- | Construct a MERGE query returning all target table columns.
mergeReturningAll ::
  (SelectList targetSelectList, FromItem source) =>
  TableDefinition targetCols targetSelectList ->
  source ->
  (QueriedRow targetSelectList -> QueriedRow (FromItemSelectList source) -> SqlExpr NormalQuery (SqlT n Bool)) ->
  NonEmpty (MergeClause targetCols targetSelectList (FromItemSelectList source)) ->
  MergeQuery (QueriedRow targetSelectList)
mergeReturningAll td src onCond clauses = Merge td src onCond clauses id

-- | Construct a WHEN MATCHED THEN ... clause.
whenMatched_ ::
  MergeClauseAction 'WhenMatched targetCols targetSelectList sourceSelectList ->
  MergeClause targetCols targetSelectList sourceSelectList
whenMatched_ = MergeWhenMatched (Nothing :: Maybe (QueriedRow t -> QueriedRow s -> SqlExpr NormalQuery (SqlT 'NonNull Bool)))

-- | Construct a WHEN MATCHED AND (condition) THEN ... clause.
whenMatchedAnd_ ::
  (QueriedRow targetSelectList -> QueriedRow sourceSelectList -> SqlExpr NormalQuery (SqlT n Bool)) ->
  MergeClauseAction 'WhenMatched targetCols targetSelectList sourceSelectList ->
  MergeClause targetCols targetSelectList sourceSelectList
whenMatchedAnd_ cond = MergeWhenMatched (Just cond)

-- | Construct a WHEN NOT MATCHED THEN ... clause.
whenNotMatched_ ::
  MergeClauseAction 'WhenNotMatched targetCols targetSelectList sourceSelectList ->
  MergeClause targetCols targetSelectList sourceSelectList
whenNotMatched_ = MergeWhenNotMatched (Nothing :: Maybe (QueriedRow s -> SqlExpr NormalQuery (SqlT 'NonNull Bool)))

-- | Construct a WHEN NOT MATCHED AND (condition) THEN ... clause.
-- The condition only receives the source row.
whenNotMatchedAnd_ ::
  (QueriedRow sourceSelectList -> SqlExpr NormalQuery (SqlT n Bool)) ->
  MergeClauseAction 'WhenNotMatched targetCols targetSelectList sourceSelectList ->
  MergeClause targetCols targetSelectList sourceSelectList
whenNotMatchedAnd_ cond = MergeWhenNotMatched (Just cond)

-- | Construct a WHEN NOT MATCHED BY SOURCE THEN ... clause.
-- Requires PostgreSQL 17 or later.
whenNotMatchedBySource_ ::
  MergeClauseAction 'WhenNotMatchedBySource targetCols targetSelectList sourceSelectList ->
  MergeClause targetCols targetSelectList sourceSelectList
whenNotMatchedBySource_ = MergeWhenNotMatchedBySource (Nothing :: Maybe (QueriedRow t -> SqlExpr NormalQuery (SqlT 'NonNull Bool)))

-- | Construct a WHEN NOT MATCHED BY SOURCE AND (condition) THEN ... clause.
-- The condition only receives the target row. Requires PostgreSQL 17 or later.
whenNotMatchedBySourceAnd_ ::
  (QueriedRow targetSelectList -> SqlExpr NormalQuery (SqlT n Bool)) ->
  MergeClauseAction 'WhenNotMatchedBySource targetCols targetSelectList sourceSelectList ->
  MergeClause targetCols targetSelectList sourceSelectList
whenNotMatchedBySourceAnd_ cond = MergeWhenNotMatchedBySource (Just cond)

writeMergeClause ::
  forall targetCols targetSelectList sourceSelectList.
  MergeClause targetCols targetSelectList sourceSelectList ->
  QueriedRow targetSelectList ->
  QueriedRow sourceSelectList ->
  WrappedRow targetCols ColumnName ->
  QueryWriter ()
writeMergeClause clause targetRow sourceRow targetColNames = case clause of
  MergeWhenMatched cond action -> do
    " WHEN MATCHED"
    writeCond (fmap (\f -> f targetRow sourceRow) cond)
    writeAction action
  MergeWhenNotMatched cond action -> do
    " WHEN NOT MATCHED"
    writeCond (fmap ($ sourceRow) cond)
    writeAction action
  MergeWhenNotMatchedBySource cond action -> do
    " WHEN NOT MATCHED BY SOURCE"
    writeCond (fmap ($ targetRow) cond)
    writeAction action
  where
    writeCond extraCond = for_ extraCond $ \e -> do
      " AND ("
      writeSyntax (unsafeGetSqlExpr e)
      ")"
    writeUpdate updates = do
      " UPDATE SET "
      writeUpdateSets updates targetColNames
    writeAction :: MergeClauseAction w targetCols targetSelectList sourceSelectList -> QueryWriter ()
    writeAction action = do
      " THEN"
      case action of
        MergeDoNothing -> " DO NOTHING"
        MergeMatchedDelete -> " DELETE"
        MergeBySourceDelete -> " DELETE"
        MergeMatchedUpdate buildUpdates -> writeUpdate (buildUpdates targetRow sourceRow)
        MergeBySourceUpdate buildUpdates -> writeUpdate (buildUpdates targetRow)
        MergeNotMatchedInsert buildInsert -> do
          let iv = toInsertValues (buildInsert sourceRow)
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

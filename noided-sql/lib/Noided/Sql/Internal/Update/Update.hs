{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Update.Update where

import Control.Monad.Trans.State.Strict
import Data.Foldable (for_)
import Noided.Sql.Internal.Class.DecodeSelectList
import Noided.Sql.Internal.Class.FromItem
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Class.SelectList
import Noided.Sql.Internal.Class.UnwrapSelectList
import Noided.Sql.Internal.Select.FromClause
import Noided.Sql.Internal.Select.SelectM
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.Syntax
import Noided.Sql.Internal.Type.TableDefinition
import Noided.Sql.Internal.Type.TableName
import Noided.Sql.Internal.Update.Sets

data UpdateQuery returning where
  Update ::
    (SelectList tableSelectList) =>
    TableDefinition tableCols tableSelectList ->
    OptionalFrom fromRow ->
    (QueriedRow tableSelectList -> fromRow -> WhereM (ColumnUpdates tableCols, returning)) ->
    UpdateQuery returning

instance Functor UpdateQuery where
  fmap f (Update td from q) = Update td from (\t r -> fmap (fmap f) (q t r))

-- | Construct an UPDATE query returning results.
--
-- The 'OptionalFrom' is the @FROM@ item (use 'noFrom_' for none, or 'crossJoin_' to combine several). Postgres does not let
-- it reference the row being updated, so the target row is not available there. The second stage gets the target row and the FROM row, and
-- may add @WHERE@ conditions (including correlated subqueries), the @SET@ values, and the @RETURNING@ value.
updateReturning_ ::
  (SelectList tableSelectList) =>
  TableDefinition tableCols tableSelectList ->
  OptionalFrom fromRow ->
  (QueriedRow tableSelectList -> fromRow -> WhereM (ColumnUpdates tableCols, returning)) ->
  UpdateQuery returning
updateReturning_ = Update

-- | Construct an UPDATE query returning nothing (actually returns (), typically used with execute_).
update_ ::
  (SelectList tableSelectList) =>
  TableDefinition tableCols tableSelectList ->
  OptionalFrom fromRow ->
  (QueriedRow tableSelectList -> fromRow -> WhereM (ColumnUpdates tableCols)) ->
  UpdateQuery ()
update_ td from q = Update td from (\t r -> (,()) <$> q t r)

updateReturningAll_ ::
  (SelectList tableSelectList) =>
  TableDefinition tableCols tableSelectList ->
  OptionalFrom fromRow ->
  ( QueriedRow tableSelectList ->
    fromRow ->
    WhereM (ColumnUpdates tableCols)
  ) ->
  UpdateQuery (QueriedRow tableSelectList)
updateReturningAll_ td from buildUpdates =
  updateReturning_ td from $ \res fr -> (,res) <$> buildUpdates res fr

writeUpdateQuery ::
  (SelectList returningList) =>
  UpdateQuery (QueriedRow returningList) ->
  QueryWriter ()
writeUpdateQuery (Update td fromM q) = do
  "UPDATE "
  writeTableName td.tableName
  " AS "
  ln <- toQuotedUniqueAlias "to_update"
  writeSyntax ln
  let targetRow = qualifyColumnNames ln td.selectedNames

  -- The FROM items are built first, without access to the target row.
  (fromRow, fromSyn) <- writeOptionalFrom fromM
  ((updates, returningList), whereState) <- runStateT (unsafeGetSelectM (unsafeGetWhereM (q targetRow fromRow))) mempty

  " SET "
  writeUpdateSets updates td.columnNames

  -- FROM clause
  for_ fromSyn $ \syn -> do
    " FROM "
    writeSyntax syn

  -- WHERE clause
  for_ (writeAnds (whereSyntaxes whereState)) $ \act -> do
    " WHERE "
    act

  " RETURNING "
  writeSelectList returningList

instance
  (SelectList returningList) =>
  Query (UpdateQuery (QueriedRow returningList))
  where
  type QuerySelectList (UpdateQuery (QueriedRow returningList)) = returningList
  writeQuerySyntax = writeUpdateQuery

instance (SelectList returningList, UnwrapSelectList returningList, DecodeSelectList returningList) => ExecutableQuery (UpdateQuery (QueriedRow returningList))

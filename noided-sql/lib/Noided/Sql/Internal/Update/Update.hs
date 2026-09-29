{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Update.Update where

import Control.Monad.Trans.State.Strict
import Data.Foldable (for_)
import Data.Sequence qualified as Seq
import Noided.Sql.Internal.Class.DecodeSelectList
import Noided.Sql.Internal.Class.FromItem
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Class.SelectList
import Noided.Sql.Internal.Class.UnwrapSelectList
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
    FromM fromRow ->
    (QueriedRow tableSelectList -> fromRow -> WhereM (ColumnUpdates tableCols, returning)) ->
    UpdateQuery returning

instance Functor UpdateQuery where
  fmap f (Update td from q) = Update td from (\t r -> fmap (fmap f) (q t r))

-- | Construct an UPDATE query returning results.
--
-- The 'FromM' action builds the @FROM@ items. Postgres does not let these reference the row being updated,
-- so it is not available there. The second stage gets the target row and the result of the 'FromM' action, and
-- may add @WHERE@ conditions (including correlated subqueries), the @SET@ values, and the @RETURNING@ value.
updateReturning ::
  (SelectList tableSelectList) =>
  TableDefinition tableCols tableSelectList ->
  FromM fromRow ->
  (QueriedRow tableSelectList -> fromRow -> WhereM (ColumnUpdates tableCols, returning)) ->
  UpdateQuery returning
updateReturning = Update

-- | Construct an UPDATE query returning nothing (actually returns (), typically used with execute_).
update ::
  (SelectList tableSelectList) =>
  TableDefinition tableCols tableSelectList ->
  FromM fromRow ->
  (QueriedRow tableSelectList -> fromRow -> WhereM (ColumnUpdates tableCols)) ->
  UpdateQuery ()
update td from q = Update td from (\t r -> (,()) <$> q t r)

updateReturningAll ::
  (SelectList tableSelectList) =>
  TableDefinition tableCols tableSelectList ->
  FromM fromRow ->
  ( QueriedRow tableSelectList ->
    fromRow ->
    WhereM (ColumnUpdates tableCols)
  ) ->
  UpdateQuery (QueriedRow tableSelectList)
updateReturningAll td from buildUpdates =
  updateReturning td from $ \res fr -> (,res) <$> buildUpdates res fr

writeUpdateQuery ::
  (SelectList returningList) =>
  UpdateQuery (QueriedRow returningList) ->
  QueryWriter ()
writeUpdateQuery (Update td fromM q) = do
  "UPDATE "
  writeTableName td.tableName
  " AS "
  ln <- toUniqueAlias "to_update"
  writeSyntax ln
  let targetRow = qualifyColumnNames ln td.selectedNames

  -- The FROM items are built first, without access to the target row.
  (fromRow, fromState) <- runStateT (unsafeGetSelectM (unsafeGetFromM fromM)) mempty
  ((updates, returningList), whereState) <- runStateT (unsafeGetSelectM (unsafeGetWhereM (q targetRow fromRow))) mempty

  " SET "
  writeUpdateSets updates td.columnNames

  -- FROM clause
  let froms = fromSyntaxes fromState
  if Seq.null froms
    then pure ()
    else do
      " FROM "
      writeSyntax $ fromCommaSepSyntax $ foldMap Written froms

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

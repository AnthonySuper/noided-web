{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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
    (QueriedRow tableSelectList -> SelectM (ColumnUpdates tableCols, returning)) ->
    UpdateQuery returning

instance Functor UpdateQuery where
  fmap f (Update td q) = Update td (\r -> fmap f <$> q r)

-- | Construct an UPDATE query returning results.
updateReturning ::
  (SelectList tableSelectList) =>
  TableDefinition tableCols tableSelectList ->
  (QueriedRow tableSelectList -> SelectM (ColumnUpdates tableCols, returning)) ->
  UpdateQuery returning
updateReturning = Update

-- | Construct an UPDATE query returning nothing (actually returns (), typically used with execute_).
update ::
  (SelectList tableSelectList) =>
  TableDefinition tableCols tableSelectList ->
  (QueriedRow tableSelectList -> SelectM (ColumnUpdates tableCols)) ->
  UpdateQuery ()
update td q = Update td (\r -> (,()) <$> q r)

writeUpdateQuery ::
  (SelectList returningList) =>
  UpdateQuery (QueriedRow returningList) ->
  QueryWriter ()
writeUpdateQuery (Update td q) = do
  "UPDATE "
  writeTableName td.tableName
  " AS "
  ln <- toUniqueAlias "to_update"
  let targetRow = qualifyColumnNames ln td.selectedNames
  
  -- Run the SelectM action to get updates, returning, and state (FROM/WHERE)
  ((updates, returningList), finalState) <- runStateT (unsafeGetSelectM (q targetRow)) mempty

  " SET "
  writeUpdateSets updates td.columnNames

  -- FROM clause
  let froms = fromSyntaxes finalState
  if Seq.null froms
    then pure ()
    else do
      " FROM "
      writeSyntax $ fromCommaSepSyntax $ foldMap Written froms

  -- WHERE clause
  for_ (writeAnds (whereSyntaxes finalState)) $ \act -> do
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

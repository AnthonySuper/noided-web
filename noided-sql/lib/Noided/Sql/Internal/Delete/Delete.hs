{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Noided.Sql.Internal.Delete.Delete where

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

data DeleteQuery returning where
  Delete ::
    (SelectList tableSelectList) =>
    TableDefinition tableCols tableSelectList ->
    OptionalFrom fromRow ->
    (QueriedRow tableSelectList -> fromRow -> WhereM returning) ->
    DeleteQuery returning

instance Functor DeleteQuery where
  fmap f (Delete td from q) = Delete td from (\t r -> fmap f (q t r))

-- | Construct a DELETE query returning results.
--
-- The 'OptionalFrom' is the @USING@ item (use 'noFrom_' for none, or 'crossJoin_' to combine several). Postgres does not let
-- it reference the row being deleted, so the target row is not available there. The second stage gets the target row and the USING row, and
-- may add @WHERE@ conditions (including correlated subqueries) and the @RETURNING@ value.
deleteReturning_ ::
  (SelectList tableSelectList) =>
  TableDefinition tableCols tableSelectList ->
  OptionalFrom fromRow ->
  (QueriedRow tableSelectList -> fromRow -> WhereM returning) ->
  DeleteQuery returning
deleteReturning_ = Delete

-- | Construct a DELETE query returning nothing.
delete_ ::
  (SelectList tableSelectList) =>
  TableDefinition tableCols tableSelectList ->
  OptionalFrom fromRow ->
  (QueriedRow tableSelectList -> fromRow -> WhereM ()) ->
  DeleteQuery ()
delete_ = Delete

writeDeleteQuery ::
  (SelectList returningList) =>
  DeleteQuery (QueriedRow returningList) ->
  QueryWriter ()
writeDeleteQuery (Delete td fromM q) = do
  "DELETE FROM "
  writeTableName td.tableName
  " AS "
  ln <- toQuotedUniqueAlias "to_delete"
  writeSyntax ln
  let targetRow = qualifyColumnNames ln td.selectedNames
  
  -- The USING items are built first, without access to the target row.
  (fromRow, fromSyn) <- writeOptionalFrom fromM
  (returningList, whereState) <- runStateT (unsafeGetSelectM (unsafeGetWhereM (q targetRow fromRow))) mempty

  -- USING clause
  for_ fromSyn $ \syn -> do
    " USING "
    writeSyntax syn

  -- WHERE clause
  for_ (writeAnds (whereSyntaxes whereState)) $ \act -> do
    " WHERE "
    act

  " RETURNING "
  writeSelectList returningList

instance
  (SelectList returningList) =>
  Query (DeleteQuery (QueriedRow returningList))
  where
  type QuerySelectList (DeleteQuery (QueriedRow returningList)) = returningList
  writeQuerySyntax = writeDeleteQuery

instance (SelectList returningList, UnwrapSelectList returningList, DecodeSelectList returningList) => ExecutableQuery (DeleteQuery (QueriedRow returningList))

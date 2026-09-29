{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Noided.Sql.Internal.Delete.Delete where

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

data DeleteQuery returning where
  Delete ::
    (SelectList tableSelectList) =>
    TableDefinition tableCols tableSelectList ->
    FromM fromRow ->
    (QueriedRow tableSelectList -> fromRow -> WhereM returning) ->
    DeleteQuery returning

instance Functor DeleteQuery where
  fmap f (Delete td from q) = Delete td from (\t r -> fmap f (q t r))

-- | Construct a DELETE query returning results.
--
-- The 'FromM' action builds the @USING@ items. Postgres does not let these reference the row being deleted,
-- so it is not available there. The second stage gets the target row and the result of the 'FromM' action, and
-- may add @WHERE@ conditions (including correlated subqueries) and the @RETURNING@ value.
deleteReturning ::
  (SelectList tableSelectList) =>
  TableDefinition tableCols tableSelectList ->
  FromM fromRow ->
  (QueriedRow tableSelectList -> fromRow -> WhereM returning) ->
  DeleteQuery returning
deleteReturning = Delete

-- | Construct a DELETE query returning nothing.
delete ::
  (SelectList tableSelectList) =>
  TableDefinition tableCols tableSelectList ->
  FromM fromRow ->
  (QueriedRow tableSelectList -> fromRow -> WhereM ()) ->
  DeleteQuery ()
delete = Delete

writeDeleteQuery ::
  (SelectList returningList) =>
  DeleteQuery (QueriedRow returningList) ->
  QueryWriter ()
writeDeleteQuery (Delete td fromM q) = do
  "DELETE FROM "
  writeTableName td.tableName
  " AS "
  ln <- toUniqueAlias "to_delete"
  writeSyntax ln
  let targetRow = qualifyColumnNames ln td.selectedNames
  
  -- The USING items are built first, without access to the target row.
  (fromRow, fromState) <- runStateT (unsafeGetSelectM (unsafeGetFromM fromM)) mempty
  (returningList, whereState) <- runStateT (unsafeGetSelectM (unsafeGetWhereM (q targetRow fromRow))) mempty

  -- USING clause (populated from fromSyntaxes)
  let froms = fromSyntaxes fromState
  if Seq.null froms
    then pure ()
    else do
      " USING "
      writeSyntax $ fromCommaSepSyntax $ foldMap Written froms

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

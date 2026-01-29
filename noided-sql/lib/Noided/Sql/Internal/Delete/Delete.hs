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
    (QueriedRow tableSelectList -> SelectM returning) ->
    DeleteQuery returning

instance Functor DeleteQuery where
  fmap f (Delete td q) = Delete td (\r -> fmap f (q r))

-- | Construct a DELETE query returning results.
deleteReturning ::
  (SelectList tableSelectList) =>
  TableDefinition tableCols tableSelectList ->
  (QueriedRow tableSelectList -> SelectM returning) ->
  DeleteQuery returning
deleteReturning = Delete

-- | Construct a DELETE query returning nothing.
delete ::
  (SelectList tableSelectList) =>
  TableDefinition tableCols tableSelectList ->
  (QueriedRow tableSelectList -> SelectM ()) ->
  DeleteQuery ()
delete td q = Delete td q

writeDeleteQuery ::
  (SelectList returningList) =>
  DeleteQuery (QueriedRow returningList) ->
  QueryWriter ()
writeDeleteQuery (Delete td q) = do
  "DELETE FROM "
  writeTableName td.tableName
  " AS "
  ln <- toUniqueAlias "to_delete"
  writeSyntax ln
  let targetRow = qualifyColumnNames ln td.selectedNames
  
  -- Run the SelectM action
  (returningList, finalState) <- runStateT (unsafeGetSelectM (q targetRow)) mempty

  -- USING clause (populated from fromSyntaxes)
  let froms = fromSyntaxes finalState
  if Seq.null froms
    then pure ()
    else do
      " USING "
      writeSyntax $ fromCommaSepSyntax $ foldMap Written froms

  -- WHERE clause
  for_ (writeAnds (whereSyntaxes finalState)) $ \act -> do
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

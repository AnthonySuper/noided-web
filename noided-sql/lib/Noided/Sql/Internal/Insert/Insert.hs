{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Insert.Insert where

import Noided.Sql.Internal.Class.DecodeSelectList
import Noided.Sql.Internal.Class.FromItem
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Class.SelectList
import Noided.Sql.Internal.Class.UnwrapSelectList
import Noided.Sql.Internal.Insert.InsertValues
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.TableDefinition
import Noided.Sql.Internal.Type.TableName

-- | Type of insert queries that return some kind of value.
data InsertQuery returning where
  Insert ::
    -- Constraints that make sure we can actually insert values
    (InsertForTable tableColumns insertedCols, SelectList tableSelectList) =>
    -- | Table to insert into
    TableDefinition tableColumns tableSelectList ->
    -- | Values to insert
    InsertValues insertedCols ->
    -- | Function to build a RETURNING list
    (QueriedRow tableSelectList -> returning) ->
    InsertQuery returning

instance Functor InsertQuery where
  fmap f (Insert td iv bf) = Insert td iv (f . bf)

insertReturning ::
  ( InsertForTable tableColumns insertedCols,
    SelectList tableSelectList
  ) =>
  TableDefinition tableColumns tableSelectList ->
  InsertValues insertedCols ->
  (QueriedRow tableSelectList -> returning) ->
  InsertQuery returning
insertReturning = Insert

insertReturningAll ::
  ( InsertForTable tableColumns insertedCols,
    SelectList tableSelectList
  ) =>
  TableDefinition tableColumns tableSelectList ->
  InsertValues insertedCols ->
  InsertQuery (QueriedRow tableSelectList)
insertReturningAll td iv = Insert td iv id

insertDefaultValuesReturning ::
  ( InsertForTable tableColumns '[],
    SelectList tableSelectList
  ) =>
  TableDefinition tableColumns tableSelectList ->
  (QueriedRow tableSelectList -> returning) ->
  InsertQuery returning
insertDefaultValuesReturning td = insertReturning td DefaultValues

writeInsertQuery ::
  (SelectList returningList) =>
  InsertQuery (QueriedRow returningList) ->
  QueryWriter ()
writeInsertQuery (Insert td iv qr) = do
  "INSERT INTO "
  writeTableName td.tableName
  " AS "
  ln <- toUniqueAlias "to_insert"
  let toUse = qualifyColumnNames ln td.selectedNames
  let returningList = qr toUse
  writeSyntax ln
  " "
  writeInsertValues iv
  " RETURNING "
  writeSelectList returningList

instance
  (SelectList returningList) =>
  Query (InsertQuery (QueriedRow returningList))
  where
  type QuerySelectList (InsertQuery (QueriedRow returningList)) = returningList
  writeQuerySyntax = writeInsertQuery

instance (SelectList returningList, UnwrapSelectList returningList, DecodeSelectList returningList) => ExecutableQuery (InsertQuery (QueriedRow returningList))

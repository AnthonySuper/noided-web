{-# LANGUAGE OverloadedRecordDot #-}

module Noided.Sql.Internal.Type.TableDefinition where

import Data.Kind (Type)
import Noided.Row
import Noided.Sql.Internal.Class.FromItem
import Noided.Sql.Internal.Class.SelectList
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.ColumnType
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.TableName

type RowLabelsInQuery ::
  [RowLabel ColumnType] -> [RowLabel SqlType]
type family RowLabelsInQuery labels where
  RowLabelsInQuery '[] = '[]
  RowLabelsInQuery (l :=> t ': rest) =
    l :=> ColumnInQuery t ': RowLabelsInQuery rest

type TableDefinition ::
  [RowLabel ColumnType] ->
  ((SqlType -> Type) -> Type) ->
  Type
data TableDefinition columnDefs selectedType
  = DefineTable
  { tableName :: !TableName,
    columnNames :: !(WrappedRow columnDefs ColumnName),
    selectedNames :: !(selectedType ColumnName)
  }

instance
  (SelectList selectedType) =>
  FromItem (TableDefinition columnDefs selectedType)
  where
  type FromItemSelectList (TableDefinition columnDefs selectedType) = selectedType
  fromItemAlias (DefineTable tn _ _) =
    tn.tableName
  fromItemLateralUsage _ = NeverLateral
  fromItemColumnAliases _ = NoColumnAliases
  writeFromItem (DefineTable tn _ _) =
    writeTableName tn
  fromItemSelectList (DefineTable _ _ rn) = rn

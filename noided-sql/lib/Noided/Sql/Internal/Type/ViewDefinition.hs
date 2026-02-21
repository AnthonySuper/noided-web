{-# LANGUAGE OverloadedRecordDot #-}

module Noided.Sql.Internal.Type.ViewDefinition where

import Data.Kind (Type)
import Noided.Sql.Internal.Class.FromItem
import Noided.Sql.Internal.Class.SelectList
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.TableName

-- | Definition of a SQL view (or any select-only result type).
--
-- Unlike 'TableDefinition', a 'ViewDef' has no column type definitions and
-- cannot be used with insert, update, delete, or merge operations — only
-- 'select'.
type ViewDef ::
  ((SqlType -> Type) -> Type) ->
  Type
data ViewDef selectedType
  = DefineView
  { viewName :: !TableName,
    viewSelectedNames :: !(selectedType ColumnName)
  }

instance
  (SelectList selectedType) =>
  FromItem (ViewDef selectedType)
  where
  type FromItemSelectList (ViewDef selectedType) = selectedType
  fromItemAlias (DefineView vn _) = vn.tableName
  fromItemLateralUsage _ = NeverLateral
  fromItemColumnAliases _ = NoColumnAliases
  writeFromItem (DefineView vn _) = writeTableName vn
  fromItemSelectList (DefineView _ rn) = rn

{-# LANGUAGE AllowAmbiguousTypes #-}

module Noided.Sql.Internal.PlainTableDef
  ( defineTableSnakeCased,
    defineTableWithNames,
    tableColumnNames,
  )
where

import Data.HKD (ffmap)
import GHC.Generics
import Noided.Row (WrappedRow)
import Noided.Sql.Internal.HKDTableDef (GSnakeCasedNames, genericSnakeCasedNames)
import Noided.Sql.Internal.Type.Col
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.TableDefinition
import Noided.Sql.Internal.Type.TableName (TableName)

-- | Build a 'TableDefinition' for a table defined with
-- 'Noided.Sql.Internal.TH.PlainTable.defineTable', naming every column after
-- its field, snake_cased.
--
-- > usersTable :: TableDefinition (TableColumns UserF) UserF
-- > usersTable = defineTableSnakeCased "users"
defineTableSnakeCased ::
  forall t.
  ( PlainTable t,
    Generic (t ColumnName),
    GSnakeCasedNames (Rep (t ColumnName))
  ) =>
  TableName ->
  TableDefinition (TableColumns t) t
defineTableSnakeCased tn =
  defineTableWithNames tn (to (genericSnakeCasedNames @(Rep (t ColumnName))))

-- | Build a 'TableDefinition' with explicit column names.
--
-- > usersTable :: TableDefinition (TableColumns UserF) UserF
-- > usersTable = defineTableWithNames "tbl_usr" UserF {id = "usr_id", name = "usr_nm"}
defineTableWithNames ::
  (PlainTable t) =>
  TableName ->
  t ColumnName ->
  TableDefinition (TableColumns t) t
defineTableWithNames tn names =
  DefineTable
    { tableName = tn,
      columnNames = tableColumnNames names,
      selectedNames = names
    }

-- | A table's column names, flattened in declaration order.
tableColumnNames :: (PlainTable t) => t ColumnName -> WrappedRow (TableColumns t) ColumnName
tableColumnNames = ffmap (MkColumnName . getColumnName . getQueryCol) . toColumnRow

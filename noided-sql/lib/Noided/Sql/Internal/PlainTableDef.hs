{-# LANGUAGE AllowAmbiguousTypes #-}

module Noided.Sql.Internal.PlainTableDef (plainTableDef) where

import GHC.Generics
import Noided.Sql.Internal.HKDTableDef (GSnakeCasedNames, genericSnakeCasedNames)
import Noided.Sql.Internal.Type.Col
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.TableDefinition
import Noided.Sql.Internal.Type.TableName (TableName)

-- | Build a 'TableDefinition' for a table defined with
-- 'Noided.Sql.Internal.TH.PlainTable.defineTable'.
--
-- > usersTable :: TableDefinition (TableColumns UserF) UserF
-- > usersTable = plainTableDef "users"
plainTableDef ::
  forall t.
  ( PlainTable t,
    Generic (t ColumnName),
    GSnakeCasedNames (Rep (t ColumnName))
  ) =>
  TableName ->
  TableDefinition (TableColumns t) t
plainTableDef tn =
  DefineTable
    { tableName = tn,
      columnNames = plainColumnNames @t,
      selectedNames = to (genericSnakeCasedNames @(Rep (t ColumnName)))
    }

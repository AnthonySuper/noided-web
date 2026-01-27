module Noided.Sql.Internal.Type.Columnar where

import Data.Kind
import Noided.Sql.Internal.Type.ColumnType
import Noided.Sql.Internal.Type.SqlType

-- | Where a column is being used.
data ColumnUsage (argument :: Type) where
  -- | To define a table.
  -- In this case, we need an argument of 'ColumnType -> Type'.
  InTableDef :: ColumnUsage (ColumnType -> Type)
  -- | In a query.
  -- In this case, we need an argument of 'SqlType -> Type'.
  InQuery :: ColumnUsage (SqlType -> Type)
  -- | In Haskell.
  -- In this case, we don't really need an argument, because
  -- this will no longer be an HKD.
  InHaskell :: ColumnUsage ()

-- | Field wrapper for columnar values.
-- This allows you to use the same data type when defining tables, in queries,
-- and in Haskell-land.
type Columnar :: ColumnType -> ColumnUsage argument -> argument -> Type
type family Columnar cd r arg where
  Columnar cd InTableDef arg = arg cd
  Columnar cd InQuery arg = arg (ColumnInQuery cd)
  Columnar cd InHaskell '() = ColumnInHaskell cd

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Noided.Sql.Internal.Type.Col
-- Description: Field wrapper for plain (realm-free) HKD tables.
--
-- Unlike 'Noided.Sql.Internal.Type.Columnar.Columnar', 'Col' only computes on
-- the static column descriptor, never on the wrapper @f@. That means
-- @Col (RegularColumn Text) f@ reduces to @f ('NonNullT' Text)@ for /any/ @f@,
-- so GHC can infer @f@ from a field's value. The column's default-ness is not
-- visible after reduction; 'Noided.Sql.Internal.TH.PlainTable.defineTable'
-- recovers it from the declared (unreduced) field type via @reify@.
module Noided.Sql.Internal.Type.Col
  ( Col,
    PlainTable (..),
  )
where

import Data.Kind (Type)
import Noided.Row
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.ColumnType
import Noided.Sql.Internal.Type.SqlType

type Col :: ColumnType -> (SqlType -> Type) -> Type
type family Col c f where
  Col (Column _ n t) f = f (SqlT n t)

-- | Tables defined with 'Noided.Sql.Internal.TH.PlainTable.defineTable'.
--
-- Carries the flattened, type-level column definitions (with defaults), which
-- are needed for INSERT / UPDATE checking.
class PlainTable (t :: (SqlType -> Type) -> Type) where
  type TableColumns t :: [RowLabel ColumnType]

  -- | Actual (snake_cased) column names, in declaration order.
  plainColumnNames :: WrappedRow (TableColumns t) ColumnName

{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Type.SelectValues where

import Data.HKD
import Data.List.NonEmpty qualified as NE
import Noided.Sql.Internal.Class.FromItem
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.Class.SelectList
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax

-- | A FROM item that results from a VALUES clause.
-- This can be used to join against a list of values, or in a MERGE statement.
data SelectValues sl where
  SelectValues :: (SelectList sl) => NE.NonEmpty (sl (SqlExpr NormalQuery)) -> SelectValues sl

instance Semigroup (SelectValues sl) where
  (SelectValues l) <> (SelectValues r) = SelectValues $ l <> r

instance (SelectList sl) => FromItem (SelectValues sl) where
  type FromItemSelectList (SelectValues sl) = sl
  fromItemAlias _ = "vals"
  fromItemLateralUsage _ = NeverLateral
  fromItemColumnAliases _ = ColumnAliases
  writeFromItem (SelectValues rows) = do
    "(VALUES "
    let rowSyns = fmap (\row -> "(" <> fromCommaSepSyntax (ffoldMap (Written . unsafeGetSqlExpr) row) <> ")") rows
    writeSyntax $ fromCommaSepSyntax $ foldMap Written rowSyns
    ")"
  fromItemSelectList _ = namedColumns

-- | Construct a 'SelectValues' FROM item from a list of rows.
selectValues_ :: (SelectList sl) => NE.NonEmpty (sl (SqlExpr NormalQuery)) -> SelectValues sl
selectValues_ = SelectValues

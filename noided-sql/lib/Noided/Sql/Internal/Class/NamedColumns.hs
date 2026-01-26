{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Class.NamedColumns where

import Control.Arrow
import Data.Functor.Const
import Data.HKD
import Data.Text (pack)
import GHC.Generics
import Noided.Row
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.Syntax
import Noided.Sql.Internal.Type.Tie

class NamedColumns hkd where
  namedColumns :: hkd ColumnName

instance (RowKnownLabels labels) => NamedColumns (WrappedRow labels) where
  namedColumns = ffmap (\(Const c) -> MkColumnName (pack c)) wrappedRowKnownLabelStrings

instance (NamedColumns l, NamedColumns r) => NamedColumns (l :*: r) where
  namedColumns = namedColumns :*: namedColumns

instance (NamedColumns l, NamedColumns r) => NamedColumns (l :-: r) where
  namedColumns = namedColumns :-: namedColumns

instance (NamedColumns (Element k)) where
  namedColumns = Element "e"

anonColumns :: (FRepeat hkd) => hkd ColumnName
anonColumns = frepeat "c"

uniqueNamedColumns :: (FTraversable hkd, NamedColumns hkd) => hkd UniqueColumnName
uniqueNamedColumns = toUniqueNames namedColumns

aliasedColumnList :: (FZip t, FTraversable t, NamedColumns t) => t (SqlExpr scope) -> Maybe Syntax
aliasedColumnList =
  fzipWith (:*:) (toUniqueNames namedColumns)
    >>> ffoldMap (\(k :*: syn) -> Written $ unsafeGetSqlExpr syn <> " AS " <> syntaxFromText (getUniqueColumnName k))
    >>> fromCommaSepWritten

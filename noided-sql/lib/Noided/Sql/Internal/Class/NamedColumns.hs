{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Class.NamedColumns
  ( NamedColumns (..),
    GNamedColumns,
    gnamedColumns,
    anonColumns,
    uniqueNamedColumns,
    aliasedColumnList,
  )
where

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
  default namedColumns ::
    (Generic (hkd ColumnName), GNamedColumns (Rep (hkd ColumnName))) =>
    hkd ColumnName
  namedColumns = gnamedColumns

-- | Exists to solve the problem described in https://blog.csongor.co.uk/opaque-constraint-synonyms/
data Opaque k

instance FFunctor Opaque where
  ffmap _ _ = error "impossible"

instance FZip Opaque where
  fzipWith _ _ = error "impossible"

instance FRepeat Opaque where
  frepeat _ = error "no"

instance {-# OVERLAPPABLE #-} (FRepeat hkd) => NamedColumns hkd where
  namedColumns = frepeat "c"

instance NamedColumns Opaque where
  namedColumns = error "impossible"

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

class GNamedColumns rep where
  genericNamedColumns :: rep ()

-- | Generically implement 'namedColumns'.
-- Column names will map one-to-one with Haskell field names.
-- Column names are quoted when used in the app, so this is fine, even if your Haskell fields are snakeCase.
gnamedColumns ::
  (Generic (hkd ColumnName), GNamedColumns (Rep (hkd ColumnName))) =>
  hkd ColumnName
gnamedColumns = to genericNamedColumns

instance (GNamedColumns f) => GNamedColumns (M1 D c f) where
  genericNamedColumns = M1 genericNamedColumns

instance (GNamedColumns f) => GNamedColumns (M1 C c f) where
  genericNamedColumns = M1 genericNamedColumns

instance (GNamedColumns l, GNamedColumns r) => GNamedColumns (l :*: r) where
  genericNamedColumns = genericNamedColumns :*: genericNamedColumns

instance GNamedColumns U1 where
  genericNamedColumns = U1

instance (Selector s) => GNamedColumns (M1 S s (K1 R (ColumnName a))) where
  genericNamedColumns =
    let name = pack $ selName (undefined :: M1 S s (K1 R (ColumnName a)) ())
     in M1 (K1 (MkColumnName name))

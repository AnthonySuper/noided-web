{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Sql.Internal.Class.SelectList (SelectList, writeSelectList) where

import Control.Arrow
import Data.Functor.Const
import Data.HKD
import Data.Kind
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax

type SelectList :: ((SqlType -> Type) -> Type) -> Constraint

-- | Opaque constraint synonym for valid select lists.
class (FTraversable selectList, NamedColumns selectList, FZip selectList) => SelectList selectList

instance (FTraversable selectList, NamedColumns selectList, FZip selectList) => SelectList selectList

-- https://blog.csongor.co.uk/opaque-constraint-synonyms/
data Opaque wrapper

instance FFunctor Opaque where
  ffmap _ _ = error "impossible"

instance FFoldable Opaque where
  ffoldMap _ _ = error "impossible"

instance FTraversable Opaque where
  ftraverse _ _ = error "impossible"

instance FZip Opaque where
  fzipWith _ _ _ = error "impossible"

instance {-# OVERLAPS #-} NamedColumns Opaque where
  namedColumns = error "impossible"

instance {-# OVERLAPS #-} SelectList Opaque

writeSelectList :: (SelectList sl) => sl (SqlExpr scope) -> QueryWriter ()
writeSelectList =
  fzipWith
    ( \uniqueName (UnsafeMkSqlExpr expr) ->
        Const $
          expr <> " AS " <> syntaxFromText (getUniqueColumnName uniqueName)
    )
    uniqueNamedColumns
    >>> ffoldMap (Written . getConst)
    >>> fromCommaSepSyntax
    >>> writeSyntax

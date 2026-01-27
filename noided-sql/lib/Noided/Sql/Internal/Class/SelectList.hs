{-# LANGUAGE UndecidableInstances #-}

module Noided.Sql.Internal.Class.SelectList (SelectList) where

import Data.HKD
import Data.Kind
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.Type.SqlType

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

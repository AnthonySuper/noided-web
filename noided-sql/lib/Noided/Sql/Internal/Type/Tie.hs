{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Type.Tie where

import Data.HKD
import GHC.Generics
import Noided.Row

infixl 4 :-:

-- | Similar to 'GHC.Generics.:*:', but associates to the left
-- instead of to the right.
--
-- This makes this much more natural to write for JOIN clauses.
data (:-:) l r a = l a :-: r a
  deriving (Show, Read, Eq, Ord, Generic, Functor, Foldable, Traversable)

instance (FFunctor l, FFunctor r) => FFunctor ((:-:) l r) where
  ffmap f (l :-: r) =
    ffmap f l :-: ffmap f r

instance (FFoldable l, FFoldable r) => FFoldable ((:-:) l r) where
  ffoldMap f (l :-: r) =
    ffoldMap f l <> ffoldMap f r

instance (FTraversable l, FTraversable r) => FTraversable ((:-:) l r) where
  ftraverse = gftraverse

instance (FRepeat l, FRepeat r) => FRepeat ((:-:) l r) where
  frepeat = gfrepeat

instance (FZip l, FZip r) => FZip ((:-:) l r) where
  fzipWith = gfzipWith

infixl 4 :--:

data (:--:) l r = l :--: r
  deriving (Show, Read, Eq, Ord, Generic)

instance (HKDUnwrap l, HKDUnwrap r) => HKDUnwrap (l :-: r) where
  type HKDUnwrapped (l :-: r) = HKDUnwrapped l :--: HKDUnwrapped r
  hkdUnwrap (l :-: r) = hkdUnwrap l :--: hkdUnwrap r

instance (HKDWrap l, HKDWrap r) => HKDWrap (l :--: r) where
  type HKDWrapped (l :--: r) = HKDWrapped l :-: HKDWrapped r
  hkdWrap (l :--: r) = hkdWrap l :-: hkdWrap r

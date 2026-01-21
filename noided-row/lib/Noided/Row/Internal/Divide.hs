{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Row.Internal.Divide where

import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Contravariant.Divisible
import Data.Functor.Identity
import Data.Vector qualified as Vec
import GHC.Exts (Any)
import Noided.Row.Internal.Type.Row
import Noided.Row.Internal.Type.WrappedRow

rowDivided ::
  forall f labels.
  (Divisible f) =>
  WrappedRow labels f ->
  f (Row labels)
rowDivided (UnsafeMkWrappedRow r) = contramap (\(MkRow wrapped) -> wrapped) singleDivide
  where
    vecDivideUnsafe v =
      case Vec.uncons v of
        Just (h, t) -> (h, t)
        Nothing -> error "Impossible: vectors had different lengths?"
    vectorDivideRec :: Vec.Vector (f Any) -> f (Vec.Vector (Identity Any))
    vectorDivideRec v =
      case Vec.uncons v of
        Nothing -> conquer
        Just (h, t) -> divide vecDivideUnsafe (contramap runIdentity h) (vectorDivideRec t)
    vectorDivide :: f (Vec.Vector (Identity Any))
    vectorDivide = vectorDivideRec r
    singleDivide :: f (WrappedRow labels Identity)
    singleDivide = contramap (\(UnsafeMkWrappedRow vec) -> vec) vectorDivide

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Row.Internal.RowToHKD where

import Data.Kind (Type)
import GHC.Generics
import GHC.TypeLits (KnownNat)
import Noided.Row.Internal.Type.RowLabel
import Noided.Row.Internal.Type.WrappedRow

class GRowToHKD (labels :: [RowLabel k]) wrapper (rep :: (Type -> Type)) where
  gRowToHKD :: WrappedRow labels wrapper -> rep a

instance (GRowToHKD labels wrapper inner) => GRowToHKD labels wrapper (C1 c inner) where
  gRowToHKD = M1 . gRowToHKD

instance (GRowToHKD labels wrapper inner) => GRowToHKD labels wrapper (D1 c inner) where
  gRowToHKD = M1 . gRowToHKD

instance (GRowToHKD labels wrapper lhs, GRowToHKD labels wrapper rhs) => GRowToHKD labels wrapper (lhs :*: rhs) where
  gRowToHKD = (:*:) <$> gRowToHKD <*> gRowToHKD

instance
  ( RowIndexOfLabel idx t ~ RowLabelsIndexByLabel label labels,
    KnownNat idx,
    lbl ~ Just label
  ) =>
  GRowToHKD labels wrapper (S1 (MetaSel lbl ignore ignore' ignore'') (Rec0 (wrapper t)))
  where
  gRowToHKD = M1 . K1 . wrappedRowGetLabel @label

class (GRowToHKD labels wrapper (Rep (hkd wrapper)), Generic (hkd wrapper)) => RepGRowToHKD labels wrapper hkd

instance (GRowToHKD labels wrapper (Rep (hkd wrapper)), Generic (hkd wrapper)) => RepGRowToHKD labels wrapper hkd

class RowToHKD (labels :: [RowLabel k]) (hkd :: (k -> Type) -> Type) where
  rowToHKD :: WrappedRow labels wrapper -> hkd wrapper

instance RowToHKD labels (WrappedRow labels) where
  rowToHKD = id

instance {-# OVERLAPPABLE #-} (forall w. RepGRowToHKD labels w hkd) => RowToHKD labels hkd where
  rowToHKD = to . gRowToHKD

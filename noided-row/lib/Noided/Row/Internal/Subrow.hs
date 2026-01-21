{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Row.Internal.Subrow where

import Data.Functor.Identity
import Data.Kind
import Noided.Row.Internal.Type.Row
import Noided.Row.Internal.Type.RowLabel
import Noided.Row.Internal.Type.WrappedRow
import Optics.Core

class Subrow (superLabels :: [RowLabel k]) (subLabels :: [RowLabel k]) where
  wrappedSubrow :: WrappedRow superLabels wrapper -> WrappedRow subLabels wrapper

instance Subrow superLabels '[] where
  wrappedSubrow _ = EmptyWrappedRow

instance (Subrow superLabels rest, RowHasLabelWithType lbl superLabels t) => Subrow superLabels (lbl :=> t ': rest) where
  wrappedSubrow = (:::%) <$> wrappedRowGetLabel @lbl <*> wrappedSubrow

subrow :: (Subrow superLabels subLabels) => Row superLabels -> Row subLabels
subrow (MkRow r) = MkRow (wrappedSubrow r)

class RecordSubrowWrapped (dt :: Type) (rl :: [RowLabel k]) (wrapper :: k -> Type) where
  recordSubrowWrapped :: dt -> WrappedRow rl wrapper

instance RecordSubrowWrapped dt '[] anyWrapper where
  recordSubrowWrapped _ = EmptyWrappedRow

instance
  (RecordSubrowWrapped dt restLabels anyWrapper, Is k A_Getter, LabelOptic lbl k dt dt (anyWrapper v) (anyWrapper v)) =>
  RecordSubrowWrapped dt (lbl :=> v ': restLabels) anyWrapper
  where
  recordSubrowWrapped = (:::%) <$> view (labelOptic @lbl) <*> recordSubrowWrapped

type RecordSubrow dt rl = RecordSubrowWrapped dt rl Identity

recordSubrow :: (RecordSubrow dt rl) => dt -> Row rl
recordSubrow = MkRow . recordSubrowWrapped

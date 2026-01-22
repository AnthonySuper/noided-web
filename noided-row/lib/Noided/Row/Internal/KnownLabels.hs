{-# LANGUAGE DataKinds #-}

module Noided.Row.Internal.KnownLabels where

import Data.Functor.Const
import Data.HKD
import Data.Proxy (Proxy (..))
import GHC.TypeLits
import Noided.Row.Internal.Type.RowLabel
import Noided.Row.Internal.Type.WrappedRow

class RowKnownLabels (labels :: [RowLabel k]) where
  wrappedRowKnownLabels :: WrappedRow labels (Const SomeSymbol)

instance RowKnownLabels '[] where
  wrappedRowKnownLabels = EmptyWrappedRow
  {-# INLINE wrappedRowKnownLabels #-}

instance (KnownSymbol label, RowKnownLabels rest) => RowKnownLabels (label :=> a ': rest) where
  wrappedRowKnownLabels = Const ss :::% wrappedRowKnownLabels
    where
      ss = SomeSymbol (Proxy @label)
  {-# INLINE wrappedRowKnownLabels #-}

data Labeled f a = WithLabel SomeSymbol (f a)
  deriving (Show, Read, Eq, Ord, Functor)

wrappedRowKnownLabelStrings :: (RowKnownLabels labels) => WrappedRow labels (Const String)
wrappedRowKnownLabelStrings = ffmap (\(Const (SomeSymbol proxy)) -> Const $ symbolVal proxy) wrappedRowKnownLabels

wrappedRowLabeled :: (RowKnownLabels labels) => WrappedRow labels wrapper -> WrappedRow labels (Labeled wrapper)
wrappedRowLabeled = fzipWith (\(Const ss) -> WithLabel ss) wrappedRowKnownLabels
{-# INLINEABLE wrappedRowLabeled #-}

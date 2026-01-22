{-# LANGUAGE DataKinds #-}

module Noided.Row.Internal.Unzip where

import Noided.Row.Internal.Type.Row
import Noided.Row.Internal.Type.RowLabel
import Noided.Row.Internal.Type.WrappedRow

class UnzipRow labels where
  unzipRow :: (Functor f) => f (Row labels) -> WrappedRow labels f

instance UnzipRow '[] where
  unzipRow _ = EmptyWrappedRow
  {-# INLINE unzipRow #-}

instance (UnzipRow r) => UnzipRow (label :=> t ': r) where
  unzipRow inF = rowH :::% unzipRow rowT
    where
      rowH = fmap (\(x ::: _) -> x) inF
      rowT = fmap (\(_ ::: r) -> r) inF
  {-# INLINE unzipRow #-}

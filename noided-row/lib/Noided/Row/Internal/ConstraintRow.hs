{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Noided.Row.Internal.ConstraintRow where

import Data.Kind
import Noided.Row.Internal.Type.RowLabel
import Noided.Row.Internal.Type.WrappedRow

-- | GADT that provides evidence for @c t@.
data ConstraintOf (c :: k -> Constraint) (t :: k) where
  Constrain :: (c t) => ConstraintOf c t

-- | A row of constraint witnesses.
-- Shows that each member of the row has an instance of a particular constraint.
class ConstraintRow (c :: k -> Constraint) (labels :: [RowLabel k]) where
  constraintRow :: WrappedRow labels (ConstraintOf c)

instance ConstraintRow c '[] where
  constraintRow = EmptyWrappedRow

instance (c k, ConstraintRow c rest) => ConstraintRow c (l :=> k ': rest) where
  constraintRow = Constrain :::% constraintRow

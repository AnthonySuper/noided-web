module Noided.Sql.Internal.Class.RangeOffset where

import Data.Int
import Data.Kind

-- | Class for Postgres types that can be used with @ RANGE @ offsets in
-- a window function.
type RangeOffset :: Type -> Constraint
class RangeOffset t where
  type RangeOffsetBound t :: Type
  type RangeOffsetBound t = t

instance RangeOffset Int64

instance RangeOffset Int32

instance RangeOffset Int16

instance RangeOffset Float

instance RangeOffset Double

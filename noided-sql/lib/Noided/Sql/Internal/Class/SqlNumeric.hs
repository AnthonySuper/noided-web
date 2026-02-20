{-# LANGUAGE TypeFamilies #-}

module Noided.Sql.Internal.Class.SqlNumeric where

import Data.Int
import Data.Kind
import Data.Scientific (Scientific)

-- | Types that are numeric in SQL and support numeric aggregate functions.
class SqlNumeric t where
  -- | The type that results from a @SUM@ operation on this type.
  type SumType t :: Type
  -- | The type that results from an @AVG@ operation on this type.
  type AvgType t :: Type

instance SqlNumeric Int16 where
  type SumType Int16 = Int64
  type AvgType Int16 = Scientific

instance SqlNumeric Int32 where
  type SumType Int32 = Int64
  type AvgType Int32 = Scientific

instance SqlNumeric Int64 where
  type SumType Int64 = Scientific
  type AvgType Int64 = Scientific

instance SqlNumeric Int where
  type SumType Int = Scientific
  type AvgType Int = Scientific

instance SqlNumeric Float where
  type SumType Float = Float
  type AvgType Float = Double

instance SqlNumeric Double where
  type SumType Double = Double
  type AvgType Double = Double

instance SqlNumeric Scientific where
  type SumType Scientific = Scientific
  type AvgType Scientific = Scientific

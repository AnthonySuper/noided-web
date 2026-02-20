{-# LANGUAGE UndecidableInstances #-}

module Noided.Sql.Internal.Type.HaskellT where

import GHC.Generics
import Noided.Sql.Internal.Class.AsHaskellValue
import Noided.Sql.Internal.Type.SqlType

newtype HaskellT wrapped = HaskT {getHaskT :: HaskellValueType wrapped}
  deriving (Generic)

deriving instance (Show (HaskellTypeOf t)) => Show (HaskellT (NonNullT t))

deriving instance (Eq (HaskellTypeOf t)) => Eq (HaskellT (NonNullT t))

deriving instance (Read (HaskellTypeOf t)) => Read (HaskellT (NonNullT t))

deriving instance (Ord (HaskellTypeOf t)) => Ord (HaskellT (NonNullT t))

deriving instance (Show (HaskellTypeOf t)) => Show (HaskellT (NullableT t))

deriving instance (Eq (HaskellTypeOf t)) => Eq (HaskellT (NullableT t))

deriving instance (Read (HaskellTypeOf t)) => Read (HaskellT (NullableT t))

deriving instance (Ord (HaskellTypeOf t)) => Ord (HaskellT (NullableT t))

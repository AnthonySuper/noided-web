{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.Form.Type.CreateOrganization where

import Data.HKD
import Data.Text (Text)
import GHC.Generics
import Noided.Form.HKD

data CreateOrganizationF wrapper
  = CreateOrganization
  { name :: wrapper (InputField Text)
  }
  deriving (Generic)

instance FFunctor CreateOrganizationF where
  ffmap = ffmapDefault

instance FFoldable CreateOrganizationF where
  ffoldMap = ffoldMapDefault

instance FTraversable CreateOrganizationF where
  ftraverse = gftraverse

instance FRepeat CreateOrganizationF where
  frepeat = gfrepeat

instance FZip CreateOrganizationF where
  fzipWith = gfzipWith

deriving via (Generically (CreateOrganizationF FormErrors)) instance Semigroup (CreateOrganizationF FormErrors)

deriving via (Generically (CreateOrganizationF FormErrors)) instance Monoid (CreateOrganizationF FormErrors)

instance HKDForm CreateOrganizationF

deriving instance (Show (wrapper (InputField Text))) => Show (CreateOrganizationF wrapper)

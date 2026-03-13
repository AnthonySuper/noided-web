{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.Form.Type.CreateItem where

import Data.HKD
import Data.Text (Text)
import GHC.Generics
import Noided.Form.HKD
import OptBeer.DB.Type.Unit

data CreateItemF wrapper
  = CreateItem
  { name :: wrapper (InputField Text),
    description :: wrapper (InputField Text),
    defaultUnit :: wrapper (InputField Unit)
  }
  deriving (Generic)

instance FFunctor CreateItemF where
  ffmap = ffmapDefault

instance FFoldable CreateItemF where
  ffoldMap = ffoldMapDefault

instance FTraversable CreateItemF where
  ftraverse = gftraverse

instance FRepeat CreateItemF where
  frepeat = gfrepeat

instance FZip CreateItemF where
  fzipWith = gfzipWith

deriving via (Generically (CreateItemF FormErrors)) instance Semigroup (CreateItemF FormErrors)

deriving via (Generically (CreateItemF FormErrors)) instance Monoid (CreateItemF FormErrors)

instance HKDForm CreateItemF

deriving instance (Show (wrapper (InputField Text)), Show (wrapper (InputField Unit))) => Show (CreateItemF wrapper)

emptyCreateItemForm :: CreateItemF FormInput
emptyCreateItemForm =
  CreateItem
    { name = InputInput NotPresent,
      description = InputInput NotPresent,
      defaultUnit = InputInput NotPresent
    }

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.Form.Type.CreateSession where

import Data.HKD
import Data.Text (Text)
import GHC.Generics
import Noided.Form.HKD
import OptBeer.Type.Hashword

data CreateSessionF wrapper
  = CreateSession
  { email :: wrapper (InputField Text),
    password :: wrapper (InputField OpaquePassword)
  }
  deriving (Generic)

instance FFunctor CreateSessionF where
  ffmap = ffmapDefault

instance FFoldable CreateSessionF where
  ffoldMap = ffoldMapDefault

instance FTraversable CreateSessionF where
  ftraverse = gftraverse

instance FRepeat CreateSessionF where
  frepeat = gfrepeat

instance FZip CreateSessionF where
  fzipWith = gfzipWith

deriving via (Generically (CreateSessionF FormErrors)) instance Semigroup (CreateSessionF FormErrors)

deriving via (Generically (CreateSessionF FormErrors)) instance Monoid (CreateSessionF FormErrors)

instance HKDForm CreateSessionF

deriving instance (Show (wrapper (InputField Text)), Show (wrapper (InputField OpaquePassword))) => Show (CreateSessionF wrapper)

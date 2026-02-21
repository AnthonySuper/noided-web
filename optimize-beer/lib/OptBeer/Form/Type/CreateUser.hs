module OptBeer.Form.Type.CreateUser where

import Data.HKD
import Data.Text (Text)
import GHC.Generics
import Noided.Form.HKD
import OptBeer.Type.Hashword

data CreateUserF wrapper
  = CreateUser
  { name :: wrapper (InputField Text),
    email :: wrapper (InputField Text),
    confirmEmail :: wrapper (InputField Text),
    password :: wrapper (InputField OpaquePassword),
    confirmPassword :: wrapper (InputField OpaquePassword)
  }
  deriving (Generic)

instance FFunctor CreateUserF where
  ffmap = ffmapDefault

instance FFoldable CreateUserF where
  ffoldMap = ffoldMapDefault

instance FTraversable CreateUserF where
  ftraverse = gftraverse

instance FRepeat CreateUserF where
  frepeat = gfrepeat

instance FZip CreateUserF where
  fzipWith = gfzipWith

deriving via (Generically (CreateUserF FormErrors)) instance Semigroup (CreateUserF FormErrors)

deriving via (Generically (CreateUserF FormErrors)) instance Monoid (CreateUserF FormErrors)

instance HKDForm CreateUserF

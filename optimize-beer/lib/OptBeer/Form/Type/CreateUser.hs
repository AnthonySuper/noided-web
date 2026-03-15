{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}

module OptBeer.Form.Type.CreateUser where

import Data.Text (Text)
import GHC.Generics
import Noided.Form.HKD
import Noided.Form.HKD.TH (defineHKDForm)
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

$(defineHKDForm ''CreateUserF)

deriving instance (Show (wrapper (InputField Text)), Show (wrapper (InputField OpaquePassword))) => Show (CreateUserF wrapper)

emptyCreateUserForm :: CreateUserF FormInput
emptyCreateUserForm =
  CreateUser
    { name = InputInput NotPresent,
      email = InputInput NotPresent,
      confirmEmail = InputInput NotPresent,
      password = InputInput NotPresent,
      confirmPassword = InputInput NotPresent
    }

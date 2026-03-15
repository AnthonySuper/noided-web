{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}

module OptBeer.Form.Type.CreateOrganization where

import Data.Text (Text)
import GHC.Generics
import Noided.Form.HKD
import Noided.Form.HKD.TH (defineHKDForm)

data CreateOrganizationF wrapper
  = CreateOrganization
  { name :: wrapper (InputField Text)
  }
  deriving (Generic)

$(defineHKDForm ''CreateOrganizationF)

deriving instance (Show (wrapper (InputField Text))) => Show (CreateOrganizationF wrapper)

emptyCreateOrganizationForm :: CreateOrganizationF FormInput
emptyCreateOrganizationForm =
  CreateOrganization
    { name = InputInput NotPresent
    }

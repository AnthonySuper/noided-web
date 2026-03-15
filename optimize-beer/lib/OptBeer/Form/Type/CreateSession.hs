{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}

module OptBeer.Form.Type.CreateSession where

import Data.Text (Text)
import GHC.Generics
import Noided.Form.HKD
import Noided.Form.HKD.TH (defineHKDForm)
import OptBeer.Type.Hashword

data CreateSessionF wrapper
  = CreateSession
  { email :: wrapper (InputField Text),
    password :: wrapper (InputField OpaquePassword)
  }
  deriving (Generic)

$(defineHKDForm ''CreateSessionF)

deriving instance (Show (wrapper (InputField Text)), Show (wrapper (InputField OpaquePassword))) => Show (CreateSessionF wrapper)


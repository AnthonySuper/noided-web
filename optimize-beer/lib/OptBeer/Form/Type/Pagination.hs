{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.Form.Type.Pagination where

import GHC.Generics
import Noided.Form.HKD
import Noided.Form.HKD.TH (defineHKDForm)

data PaginationFormF wrapper
  = PaginationForm
  { page :: wrapper (InputField Int),
    perPage :: wrapper (InputField Int)
  }
  deriving (Generic)

$(defineHKDForm ''PaginationFormF)

deriving instance
  (Show (wrapper (InputField Int))) =>
  Show (PaginationFormF wrapper)

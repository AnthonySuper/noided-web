{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.Form.Type.Item where

import Data.Text (Text)
import GHC.Generics
import Noided.Form.HKD
import Noided.Form.HKD.TH (defineHKDForm)
import OptBeer.DB.Type.Unit

data ItemFormF wrapper
  = ItemForm
  { name :: wrapper (InputField Text),
    description :: wrapper (InputField Text),
    defaultUnit :: wrapper (InputField Unit)
  }
  deriving (Generic)

$(defineHKDForm ''ItemFormF)

deriving instance (Show (wrapper (InputField Text)), Show (wrapper (InputField Unit))) => Show (ItemFormF wrapper)

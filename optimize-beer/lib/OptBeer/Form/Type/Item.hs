{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}

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

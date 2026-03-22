{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.Form.Type.Recipe where

import Data.Int (Int32)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics
import Noided.Form.HKD
import Noided.Form.HKD.TH (defineHKDForm)
import OptBeer.DB.Type.Unit

data RecipeFormF wrapper
  = RecipeForm
  { name :: wrapper (InputField Text),
    description :: wrapper (InputField Text),
    batchSize :: wrapper (InputField Scientific),
    batchSizeUnit :: wrapper (InputField Unit),
    targetOg :: wrapper (InputField Scientific),
    targetFg :: wrapper (InputField Scientific),
    targetAbv :: wrapper (InputField Scientific),
    targetIbu :: wrapper (InputField Int32),
    targetSrm :: wrapper (InputField Scientific),
    boilTimeMinutes :: wrapper (InputField Int32),
    targetEfficiency :: wrapper (InputField Scientific)
  }
  deriving (Generic)

$(defineHKDForm ''RecipeFormF)

deriving instance
  ( Show (wrapper (InputField Text)),
    Show (wrapper (InputField Scientific)),
    Show (wrapper (InputField Unit)),
    Show (wrapper (InputField Int32))
  ) =>
  Show (RecipeFormF wrapper)

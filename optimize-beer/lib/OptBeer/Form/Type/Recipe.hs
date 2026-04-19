{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.Form.Type.Recipe where

import Data.Int (Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics
import Noided.Form.HKD
import Noided.Form.HKD.TH (defineHKDForm)
import OptBeer.DB.Type.RecipeStage
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
    targetEfficiency :: wrapper (InputField Scientific),
    -- a new ingredient is added if there's a new item in the array
    -- which we can probably do with an HTML dialog element I guess?
    ingredients :: wrapper (ListField (SubformField RecipeIngredientFormF))
  }
  deriving (Generic)

deriving instance
  ( Show (wrapper (InputField Int64)),
    Show (wrapper (InputField Scientific)),
    Show (wrapper (InputField Unit)),
    Show (wrapper (InputField RecipeStage)),
    Show (wrapper (InputField Int32)),
    Show (wrapper (InputField Text))
  ) =>
  Show (RecipeIngredientFormF wrapper)

deriving instance
  ( Show (wrapper (InputField Text)),
    Show (wrapper (InputField Scientific)),
    Show (wrapper (InputField Unit)),
    Show (wrapper (InputField Int32)),
    Show (wrapper (ListField (SubformField RecipeIngredientFormF)))
  ) =>
  Show (RecipeFormF wrapper)

data RecipeIngredientFormF wrapper
  = RecipeIngredientForm
  { itemId :: wrapper (InputField Int64),
    amount :: wrapper (InputField Scientific),
    amountUnit :: wrapper (InputField Unit),
    additionStage :: wrapper (InputField RecipeStage),
    additionTimeMinutes :: wrapper (InputField Int32),
    notes :: wrapper (InputField Text)
  }
  deriving (Generic)

$(defineHKDForm ''RecipeIngredientFormF)
$(defineHKDForm ''RecipeFormF)

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.DB.Table.RecipeStep where

import Data.Int (Int32)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics
import Noided.Sql.Define
import OptBeer.DB.Ids.RecipeId
import OptBeer.DB.Ids.RecipeStepId
import OptBeer.DB.Table.Timestamps
import OptBeer.DB.Type.RecipeStage

data RecipeStepF realm wrapper
  = RecipeStep
  { id :: Columnar (IdentityColumn RecipeStepId) realm wrapper,
    recipeId :: Columnar (RegularColumn RecipeId) realm wrapper,
    stepNumber :: Columnar (RegularColumn Int32) realm wrapper,
    stepStage :: Columnar (Column MayBeDefault NonNull RecipeStage) realm wrapper,
    stepDescription :: Columnar (Column MayBeDefault NonNull Text) realm wrapper,
    
    -- Functional step data
    durationMinutes :: Columnar (Column NoDefault Nullable Int32) realm wrapper,
    temperatureCelsius :: Columnar (Column NoDefault Nullable Scientific) realm wrapper,
    
    timestamps :: TimestampsF realm wrapper
  }
  deriving (Generic)

$(defineHKDTable ''RecipeStepF)

deriving instance Show RecipeStep

deriving instance Eq RecipeStep

deriving instance Ord RecipeStep

recipeStepsTable :: HKDTableDef RecipeStepF
recipeStepsTable = hkdTableDef @RecipeStepF "recipe_steps"

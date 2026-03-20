{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.DB.Table.RecipeIngredient where

import Data.Int (Int32)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics
import Noided.Sql.Define
import OptBeer.DB.Ids.ItemId
import OptBeer.DB.Ids.RecipeId
import OptBeer.DB.Table.Timestamps
import OptBeer.DB.Type.RecipeStage
import OptBeer.DB.Type.Unit
import OptBeer.DB.Type.UnitCategory

data RecipeIngredientF realm wrapper
  = RecipeIngredient
  { recipeId :: Columnar (RegularColumn RecipeId) realm wrapper,
    itemId :: Columnar (RegularColumn ItemId) realm wrapper,
    amount :: Columnar (RegularColumn Scientific) realm wrapper,
    amountUnit :: Columnar (RegularColumn Unit) realm wrapper,
    amountUnitCategory :: Columnar (Column AlwaysDefault NonNull UnitCategory) realm wrapper,
    amountNormalized :: Columnar (Column AlwaysDefault NonNull Scientific) realm wrapper,
    
    additionStage :: Columnar (Column MayBeDefault NonNull RecipeStage) realm wrapper,
    additionTimeMinutes :: Columnar (Column NoDefault Nullable Int32) realm wrapper,
    
    notes :: Columnar (Column MayBeDefault NonNull Text) realm wrapper,
    timestamps :: TimestampsF realm wrapper
  }
  deriving (Generic)

$(defineHKDTable ''RecipeIngredientF)

deriving instance Show RecipeIngredient

deriving instance Eq RecipeIngredient

deriving instance Ord RecipeIngredient

recipeIngredientsTable :: HKDTableDef RecipeIngredientF
recipeIngredientsTable = hkdTableDef @RecipeIngredientF "recipe_ingredients"

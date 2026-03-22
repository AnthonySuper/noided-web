{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.Form.Validate.Recipe where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Noided.Form.HKD
import Noided.Sql
import Noided.Validation
import OptBeer.DB.Ids.OrganizationId
import OptBeer.DB.Ids.RecipeId
import OptBeer.DB.Table.Recipe
import OptBeer.DB.Type.Unit (unitCategory)
import OptBeer.DB.Type.UnitCategory (UnitCategory (Volume))
import OptBeer.Form.Type.Recipe
import OptBeer.ValidationError.BadUnitCategory
import OptBeer.ValidationError.ValueTaken

recipeValidator :: OrganizationId -> Maybe RecipeId -> FormValidator (TransactM e) (SubformField RecipeFormF)
recipeValidator orgId mRecipeId = validateSubform $
  RecipeForm
    { name = validateRecipeName orgId mRecipeId,
      description = validateInput return,
      batchSize = validateInput $ \val -> do
        when (val <= 0) $
          failNonfatal $ TooSmall (0 :: Scientific)
        return val,
      batchSizeUnit = validateInput $ \val -> do
        when (unitCategory val /= Volume) $
          failNonfatal $ BadUnitCategory Volume
        return val,
      targetOg = validateInput return,
      targetFg = validateInput return,
      targetAbv = validateInput return,
      targetIbu = validateInput return,
      targetSrm = validateInput return,
      boilTimeMinutes = validateInput return,
      targetEfficiency = validateInput return
    }

validateRecipeName :: OrganizationId -> Maybe RecipeId -> FormValidator (TransactM e) (InputField Text)
validateRecipeName orgId mRecipeId = validateInput $ \nameText -> do
  let stripped = T.strip nameText
  when (T.null stripped) $
    failNonfatal Blank

  exists <- lift $ queryMaybe $ do
    row <- addFrom_ (fromBase_ recipesTable)
    addWhere_ (row.organizationId ==. bindParam orgId)
    addWhere_ (row.name ==. bindParam stripped)
    case mRecipeId of
      Just recipeId -> addWhere_ (row.id /=. bindParam recipeId)
      Nothing -> return ()
    select_ $ Element row.name
  
  case exists of
    Just _ -> failNonfatal ValueTaken
    Nothing -> return ()
  
  return stripped

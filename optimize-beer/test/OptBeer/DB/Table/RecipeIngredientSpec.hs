{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.DB.Table.RecipeIngredientSpec (spec) where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Noided.Row
import Noided.Sql
import OptBeer.DB.Table.Item qualified as Item
import OptBeer.DB.Table.Organization qualified as Org
import OptBeer.DB.Table.Recipe qualified as Recipe
import OptBeer.DB.Table.RecipeIngredient
import OptBeer.DB.Table.SpecHelper
import OptBeer.DB.Type.Unit
import OptBeer.DB.Type.UnitCategory
import Test.Hspec

spec :: ConnectionSpec
spec = do
  it "has a valid table def" $
    assertValidTableDef recipeIngredientsTable

  describe "generated columns" $ do
    it "correctly sets amountUnitCategory and amountNormalized" $ \pool -> do
      (category, normalized) <- flip (runDB @()) pool $ do
        org <-
          querySingleRow $
            insertReturningAll Org.organizationsTable $
              singleValue_ (#name :==> mutateVal_ (bindParam ("Test Org" :: Text)) :::%? EmptyWrappedRow)

        item <-
          querySingleRow $
            insertReturningAll Item.itemsTable $
              singleValue_
                ( #organizationId :==> mutateVal_ (bindParam org.id)
                    :::%? #name :==> mutateVal_ (bindParam ("Hops" :: Text))
                    :::%? #description :==> defaultVal_
                    :::%? #defaultUnit :==> mutateVal_ (bindParam Gram)
                    :::%? EmptyWrappedRow
                )

        recipe <-
          querySingleRow $
            insertReturningAll Recipe.recipesTable $
              singleValue_
                ( #organizationId :==> mutateVal_ (bindParam org.id)
                    :::%? #name :==> mutateVal_ (bindParam ("Test Recipe" :: Text))
                    :::%? #batchSize :==> mutateVal_ (bindParam (20 :: Scientific))
                    :::%? #batchSizeUnit :==> mutateVal_ (bindParam Liter)
                    :::%? EmptyWrappedRow
                )

        ingredient <-
          querySingleRow $
            insertReturningAll recipeIngredientsTable $
              singleValue_
                ( #recipeId :==> mutateVal_ (bindParam recipe.id)
                    :::%? #itemId :==> mutateVal_ (bindParam item.id)
                    :::%? #amount :==> mutateVal_ (bindParam (100 :: Scientific))
                    :::%? #amountUnit :==> mutateVal_ (bindParam Gram)
                    :::%? EmptyWrappedRow
                )
        return (ingredient.amountUnitCategory, ingredient.amountNormalized)

      category `shouldBe` Mass
      normalized `shouldBe` 100

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.DB.Table.RecipeStepSpec (spec) where

import Data.Int (Int32)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Noided.Row
import Noided.Sql
import OptBeer.DB.Table.Organization qualified as Org
import OptBeer.DB.Table.Recipe qualified as Recipe
import OptBeer.DB.Table.RecipeStep
import OptBeer.DB.Table.SpecHelper
import OptBeer.DB.Type.RecipeStage
import OptBeer.DB.Type.Unit
import Test.Hspec

spec :: ConnectionSpec
spec = do
  it "has a valid table def" $
    assertValidTableDef recipeStepsTable

  it "inserts a step correctly" $ \pool -> do
    step <- flip (runDB @()) pool $ do
      org <-
        querySingleRow $
          insertReturningAll Org.organizationsTable $
            singleValue_ (#name :==> mutateVal_ (bindParam ("Test Org" :: Text)) :::%? EmptyWrappedRow)

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

      querySingleRow $
        insertReturningAll recipeStepsTable $
          singleValue_
            ( #recipeId :==> mutateVal_ (bindParam recipe.id)
                :::%? #stepNumber :==> mutateVal_ (bindParam (1 :: Int32))
                :::%? #stepStage :==> mutateVal_ (bindParam Mash)
                :::%? #stepDescription :==> mutateVal_ (bindParam ("Mash in" :: Text))
                :::%? #durationMinutes :==> mutateVal_ (bindParam (Just (60 :: Int32)))
                :::%? #temperatureCelsius :==> mutateVal_ (bindParam (Just (65 :: Scientific)))
                :::%? EmptyWrappedRow
            )
    step.stepStage `shouldBe` Mash
    step.stepNumber `shouldBe` 1

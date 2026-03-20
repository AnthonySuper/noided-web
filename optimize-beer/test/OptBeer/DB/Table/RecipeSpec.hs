{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.DB.Table.RecipeSpec (spec) where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Noided.Row
import Noided.Sql
import OptBeer.DB.Table.Organization qualified as Org
import OptBeer.DB.Table.Recipe
import OptBeer.DB.Table.SpecHelper
import OptBeer.DB.Type.Unit
import Test.Hspec

spec :: ConnectionSpec
spec = do
  it "has a valid table def" $
    assertValidTableDef recipesTable

  describe "generated column: batchSizeNormalized" $ do
    it "correctly normalizes batch size (20L -> 20000ml)" $ \pool -> do
      normalized <- flip (runDB @()) pool $ do
        org <-
          querySingleRow $
            insertReturningAll Org.organizationsTable $
              singleValue_ (#name :==> mutateVal_ (bindParam ("Test Org" :: Text)) :::%? EmptyWrappedRow)

        recipe <-
          querySingleRow $
            insertReturningAll recipesTable $
              singleValue_
                ( #organizationId :==> mutateVal_ (bindParam org.id)
                    :::%? #name :==> mutateVal_ (bindParam ("Test Recipe" :: Text))
                    :::%? #batchSize :==> mutateVal_ (bindParam (20 :: Scientific))
                    :::%? #batchSizeUnit :==> mutateVal_ (bindParam Liter)
                    :::%? EmptyWrappedRow
                )
        return recipe.batchSizeNormalized
      normalized `shouldBe` 20000

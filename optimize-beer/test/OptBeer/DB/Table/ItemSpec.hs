module OptBeer.DB.Table.ItemSpec (spec) where

import Data.Text (Text)
import Noided.Row
import Noided.Sql
import OptBeer.DB.Table.Item
import OptBeer.DB.Table.Organization qualified as Org
import OptBeer.DB.Table.SpecHelper
import OptBeer.DB.Type.Unit
import OptBeer.DB.Type.UnitCategory
import Test.Hspec

spec :: ConnectionSpec
spec = do
  it "has a valid table def" $
    assertValidTableDef itemsTable

  describe "generated column: measureCategory" $ do
    it "correctly sets measureCategory for a mass unit" $ \pool -> do
      category <- flip (runDB @()) pool $ do
        org <- querySingleRow $
          insertReturningAll Org.organizationsTable $
            singleValue_ (#name :==> mutateVal_ (bindParam ("Test Org" :: Text)) :::%? EmptyWrappedRow)

        item <- querySingleRow $
          insertReturningAll itemsTable $
            singleValue_
              ( #organizationId :==> mutateVal_ (bindParam org.id)
              :::%? #name :==> mutateVal_ (bindParam ("Hops" :: Text))
              :::%? #description :==> defaultVal_
              :::%? #defaultUnit :==> mutateVal_ (bindParam Gram)
              :::%? EmptyWrappedRow
              )
        return item.measureCategory
      category `shouldBe` Mass

    it "correctly sets measureCategory for a volume unit" $ \pool -> do
      category <- flip (runDB @()) pool $ do
        org <- querySingleRow $
          insertReturningAll Org.organizationsTable $
            singleValue_ (#name :==> mutateVal_ (bindParam ("Test Org 2" :: Text)) :::%? EmptyWrappedRow)

        item <- querySingleRow $
          insertReturningAll itemsTable $
            singleValue_
              ( #organizationId :==> mutateVal_ (bindParam org.id)
              :::%? #name :==> mutateVal_ (bindParam ("Water" :: Text))
              :::%? #description :==> defaultVal_
              :::%? #defaultUnit :==> mutateVal_ (bindParam Liter)
              :::%? EmptyWrappedRow
              )
        return item.measureCategory
      category `shouldBe` Volume

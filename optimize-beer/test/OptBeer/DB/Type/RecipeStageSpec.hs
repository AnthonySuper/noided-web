module OptBeer.DB.Type.RecipeStageSpec (spec) where

import OptBeer.DB.Type.RecipeStage
import OptBeer.DB.Table.SpecHelper
import Test.Hspec

spec :: ConnectionSpec
spec = describe "RecipeStage" $ do
  it "correctly round-trips all enum values" $ \pool ->
    assertEnumRoundtrips @RecipeStage pool

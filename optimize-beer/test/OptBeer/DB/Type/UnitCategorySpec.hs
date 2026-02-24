module OptBeer.DB.Type.UnitCategorySpec (spec) where

import OptBeer.DB.Type.UnitCategory
import OptBeer.DB.Table.SpecHelper
import Test.Hspec

spec :: ConnectionSpec
spec = describe "UnitCategory" $ do
  it "correctly round-trips all enum values" $ \pool ->
    assertEnumRoundtrips @UnitCategory pool

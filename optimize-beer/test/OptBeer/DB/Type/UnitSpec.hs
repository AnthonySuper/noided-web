module OptBeer.DB.Type.UnitSpec (spec) where

import OptBeer.DB.Type.Unit
import OptBeer.DB.Table.SpecHelper
import Test.Hspec

spec :: ConnectionSpec
spec = describe "Unit" $ do
  it "correctly round-trips all enum values" $ \pool ->
    assertEnumRoundtrips @Unit pool

module OptBeer.DB.Type.OrganizationAccessLevelSpec (spec) where

import OptBeer.DB.Type.OrganizationAccessLevel
import OptBeer.DB.Table.SpecHelper
import Test.Hspec

spec :: ConnectionSpec
spec = describe "OrganizationAccessLevel" $ do
  it "correctly round-trips all enum values" $ \pool ->
    assertEnumRoundtrips @OrganizationAccessLevel pool

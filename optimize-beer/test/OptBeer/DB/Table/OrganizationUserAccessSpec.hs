module OptBeer.DB.Table.OrganizationUserAccessSpec (spec) where

import OptBeer.DB.Table.OrganizationUserAccess
import OptBeer.DB.Table.SpecHelper
import Test.Hspec

spec :: ConnectionSpec
spec = do
  it "has a valid table def" $
    assertValidTableDef organizationUserAccessesTable

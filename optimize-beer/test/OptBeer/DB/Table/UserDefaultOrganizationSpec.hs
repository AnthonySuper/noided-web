module OptBeer.DB.Table.UserDefaultOrganizationSpec (spec) where

import OptBeer.DB.Table.UserDefaultOrganization
import OptBeer.DB.Table.SpecHelper
import Test.Hspec

spec :: ConnectionSpec
spec = do
  it "has a valid table def" $
    assertValidTableDef userDefaultOrganizationsTable

module OptBeer.DB.Table.OrganizationSpec (spec) where

import OptBeer.DB.Table.Organization
import OptBeer.DB.Table.SpecHelper
import Test.Hspec

spec :: ConnectionSpec
spec = do
  it "has a valid table def" $
    assertValidTableDef organizationsTable

module OptBeer.DB.Table.UserPasswordSpec (spec) where

import OptBeer.DB.Table.UserPassword
import OptBeer.DB.Table.SpecHelper
import Test.Hspec

spec :: ConnectionSpec
spec = do
  it "has a valid table def" $
    assertValidTableDef userPasswordsTable

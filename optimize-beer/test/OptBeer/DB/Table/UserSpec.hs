module OptBeer.DB.Table.UserSpec (spec) where

import OptBeer.DB.Table.User
import OptBeer.DB.Table.SpecHelper
import Test.Hspec

spec :: ConnectionSpec
spec = do
  it "has a valid table def" $
    assertValidTableDef usersTable

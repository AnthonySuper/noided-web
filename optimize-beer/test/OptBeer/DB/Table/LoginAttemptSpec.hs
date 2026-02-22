module OptBeer.DB.Table.LoginAttemptSpec (spec) where

import OptBeer.DB.Table.LoginAttempt
import OptBeer.DB.Table.SpecHelper
import Test.Hspec

spec :: ConnectionSpec
spec = do
  it "has a valid table def" $
    assertValidTableDef loginAttemptsTable

module OptBeer.DB.Table.SessionSpec (spec) where

import OptBeer.DB.Table.Session
import OptBeer.DB.Table.SpecHelper
import Test.Hspec

spec :: ConnectionSpec
spec = do
  it "has a valid table def" $
    assertValidTableDef sessionsTable

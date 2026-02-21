module OptBeer.DB.Table.ActorSpec (spec) where

import OptBeer.DB.Table.Actor
import OptBeer.DB.Table.SpecHelper
import Test.Hspec

spec :: ConnectionSpec
spec = do
  it "has a valid table def" $
    assertValidTableDef actorsTable

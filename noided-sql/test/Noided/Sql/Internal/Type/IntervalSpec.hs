module Noided.Sql.Internal.Type.IntervalSpec (spec) where

import Data.ByteString qualified as BS
import Noided.Sql.Internal.Type.Interval
import Test.Hspec

spec :: Spec
spec = describe "Interval binary codec" $ do
  it "round-trips months, days and microseconds" $ do
    let iv = Interval 14 (-3) 123456789
    intervalFromBinary (intervalToBinary iv) `shouldBe` Right iv

  it "round-trips negative values" $ do
    let iv = Interval (-1) (-2) (-3)
    intervalFromBinary (intervalToBinary iv) `shouldBe` Right iv

  it "uses Postgres wire layout (int64 micros, int32 days, int32 months)" $ do
    BS.unpack (intervalToBinary (Interval 1 2 3))
      `shouldBe` [0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 2, 0, 0, 0, 1]

  it "rejects the wrong length" $ do
    intervalFromBinary (BS.pack [1, 2, 3]) `shouldSatisfy` either (const True) (const False)

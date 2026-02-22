module OptBeer.Action.UserSpec (spec) where

import OptBeer.Action.SpecHelper
import Test.Hspec

createUserSpec :: TransactingSpec
createUserSpec = describe "createUserAction" $ do
  describe "with good parameters" $ do
    it "creates a new user and redirects" $ \_ -> do
      _ <- fail "TODO: implement me!"
      "is implemted" `shouldBe` ("True" :: String)
    return ()
  return ()

spec :: TransactingSpec
spec = do
  createUserSpec

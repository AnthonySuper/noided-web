{-# LANGUAGE OverloadedStrings #-}

module Noided.Server.Internal.Type.VerbRouterSpec (spec) where

import Noided.Server.Internal.Type.Verb
import Noided.Server.Internal.Type.VerbRouter
import Optics.Core
import Prelude hiding (lookup)
import Test.Hspec

spec :: Spec
spec = do
  describe "Semigroup" $ do
    it "merges with left bias" $ do
      let r1 = singleton GET ("left" :: String)
      let r2 = singleton GET "right"
      let combined = r1 <> r2
      lookup GET combined `shouldBe` Just "left"

    it "merges disjoint verbs" $ do
      let r1 = singleton GET ("get" :: String)
      let r2 = singleton POST "post"
      let combined = r1 <> r2
      lookup GET combined `shouldBe` Just "get"
      lookup POST combined `shouldBe` Just "post"

  describe "At instance" $ do
    it "sets values" $ do
      let r = mempty & at GET ?~ ("got" :: String)
      lookup GET r `shouldBe` Just "got"

    it "overwrites values" $ do
      let r = singleton GET ("old" :: String) & at GET ?~ "new"
      lookup GET r `shouldBe` Just "new"

    it "deletes values" $ do
      let r = singleton GET ("old" :: String) & at GET .~ Nothing
      lookup GET r `shouldBe` Nothing

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Translate.Internal.RenderSpec (spec) where

import Noided.Translate
import Test.Hspec

pluralSpec :: Spec
pluralSpec = describe "using pluralize" $ do
  let doRender = renderMessage "{pluralize ($v) { one { 1 Pound } default { $v Pounds }}}"
  it "works with no values" $
    doRender mempty `shouldBe` "$v Pounds "
  it "works with a single value" $
    doRender [("v", ParamInt 1)]
      `shouldBe` "1 Pound "
  it "works with a plural value" $
    doRender [("v", ParamFloat 10)]
      `shouldBe` "10.0 Pounds "

spec :: Spec
spec = do
  pluralSpec
  describe "rendering variables" $ do
    it "renders missing variables as their raw name" $
      renderMessage "Hello $username" [] `shouldBe` "Hello $username"

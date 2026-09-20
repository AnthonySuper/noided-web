{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Translate.Internal.RenderSpec (spec) where

import Data.Text (Text)
import Noided.Translate
import Noided.Translate.SpecHelper
import Test.Hspec

pluralSpec :: Spec
pluralSpec = describe "using pluralize" $ do
  -- Note that this message has no @many@ arm, so anything that is not a single
  -- item falls through to @default@.
  let msg :: Text
      msg = "{pluralize ($v) { one { 1 Pound } default { $v Pounds }}}"
  it "falls back to the default clause when the variable is missing" $
    msg `rendersTo` "$v Pounds "
  it "uses the one clause for a single value" $
    rendersWith msg [("v", ParamInt 1)] "1 Pound "
  it "falls back to the default clause for a plural value" $
    rendersWith msg [("v", ParamFloat 10)] "10.0 Pounds "

spec :: Spec
spec = do
  pluralSpec
  describe "rendering variables" $
    it "renders missing variables as their raw name" $
      "Hello $username" `rendersTo` "Hello $username"

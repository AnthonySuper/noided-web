{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Web.Html.Internal.Type.FormContextSpec (spec) where

import Noided.Form
import Noided.Form.HKD
import Noided.Web.Html.Internal.Type.FormContext
import Optics.Core
import Test.Hspec

modelContextSpec :: Spec
modelContextSpec = describe "contexts with a single model name and attribute name" $ do
  let formContext = mempty & #modelNames .~ ["User"]
  let rc = RenderContext (InputInput @Int $ NotPresent) mempty (MkFormCanonicalKey [CanonicalObjectPiece "name"])
  let fieldCtx = FieldCtx formContext rc
  it "has good attribute names" $
    fieldContextAttributeNameKeys fieldCtx
      `shouldBe` [ "form.User.attributes.name.name",
                   "form.attributes.name.name"
                 ]
  it "has good base error prefixes" $
    fieldContextAttributeErrorTranslationPrefixes fieldCtx
      `shouldBe` [ "form.User.attributes.name.errors",
                   "form.User.errors",
                   "form.errors",
                   "errors"
                 ]
  it "has good form base error prefixes" $
    fieldContextBaseErrorTranslationPrefixes fieldCtx
      `shouldBe` [ "form.User.base.errors",
                   "form.errors",
                   "errors"
                 ]

noModelContextSpec :: Spec
noModelContextSpec = describe "contexts with no model name" $ do
  let formContext = mempty
  let rc = RenderContext (InputInput @Int $ NotPresent) mempty (MkFormCanonicalKey [CanonicalObjectPiece "name"])
  let fieldCtx = FieldCtx formContext rc
  it "has good attribute names" $
    fieldContextAttributeNameKeys fieldCtx
      `shouldBe` [ "form.attributes.name.name"
                 ]
  it "has good base error prefixes" $
    fieldContextAttributeErrorTranslationPrefixes fieldCtx
      `shouldBe` [ "form.errors",
                   "errors"
                 ]
  it "has good form base error prefixes" $
    fieldContextBaseErrorTranslationPrefixes fieldCtx
      `shouldBe` [ "form.errors",
                   "errors"
                 ]

spec :: Spec
spec = do
  modelContextSpec
  noModelContextSpec

{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Form.Render.CreateItemSpec (spec) where

import Data.Functor.Identity (Identity)
import Lucid.Base (HtmlT)
import Noided.Form.HKD
import Noided.Translate
import Noided.Web.Html.FormRender
import Noided.Web.Html (TranslationT)
import OptBeer.Form.Render.CreateItem
import OptBeer.Form.Render.SpecHelper
import OptBeer.Form.Type.CreateItem
import Test.Hspec

spec :: SpecWith Translations
spec = describe "createItemRenderer" $ do
  withTranslationsInLocale "en" $ do
    it "renders without bad translations" $ \runner -> do
      let input =
            CreateItem
              { name = InputInput NotPresent,
                description = InputInput NotPresent,
                defaultUnit = InputInput NotPresent
              }
          errs = mempty
          renderAct :: HtmlT (TranslationT Identity) ()
          renderAct = renderFormT createItemRenderer input errs
          soup = runTranslationToSoup runner renderAct
      assertHasNoBadTranslations soup

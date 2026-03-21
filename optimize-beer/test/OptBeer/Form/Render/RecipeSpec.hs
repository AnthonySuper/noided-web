{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Form.Render.RecipeSpec (spec) where

import Data.Functor.Identity (Identity)
import Lucid.Base (HtmlT)
import Noided.Form.HKD
import Noided.Translate
import Noided.Web.Html (TranslationT)
import Noided.Web.Html.FormRender
import OptBeer.Form.Render.Recipe
import OptBeer.Form.Render.SpecHelper
import Test.Hspec

spec :: SpecWith Translations
spec = describe "recipeRenderer" $ do
  withTranslationsInLocale "en" $ do
    it "renders without bad translations" $ \runner -> do
      let input = hkdFormEmpty
          errs = mempty
          renderAct :: HtmlT (TranslationT Identity) ()
          renderAct = renderFormT recipeRenderer input errs
          soup = runTranslationToSoup runner renderAct
      assertHasNoBadTranslations soup

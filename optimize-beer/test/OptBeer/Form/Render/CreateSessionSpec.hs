{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Form.Render.CreateSessionSpec (spec) where

import Data.Functor.Identity (Identity)
import Lucid.Base (HtmlT)
import Noided.Form.HKD
import Noided.Translate
import Noided.Web.Html.FormRender
import Noided.Web.Html (TranslationT)
import OptBeer.Form.Render.CreateSession
import OptBeer.Form.Render.SpecHelper
import OptBeer.Form.Type.CreateSession
import Test.Hspec

spec :: SpecWith Translations
spec = describe "createSessionRenderer" $ do
  withTranslationsInLocale "en" $ do
    it "renders without bad translations" $ \runner -> do
      let input =
            CreateSession
              { email = InputInput NotPresent,
                password = InputInput NotPresent
              }
          errs = mempty
          renderAct :: HtmlT (TranslationT Identity) ()
          renderAct = renderFormT createSessionRenderer input errs
          soup = runTranslationToSoup runner renderAct
      assertHasNoBadTranslations soup

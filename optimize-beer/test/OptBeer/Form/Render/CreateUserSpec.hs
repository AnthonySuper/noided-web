{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Form.Render.CreateUserSpec (spec) where

import Data.Functor.Identity (Identity)
import Lucid.Base (HtmlT)
import Noided.Form.HKD
import Noided.Translate
import Noided.Web.Html.FormRender
import Noided.Web.Html (TranslationT)
import OptBeer.Form.Render.CreateUser
import OptBeer.Form.Render.SpecHelper
import OptBeer.Form.Type.CreateUser
import Test.Hspec

spec :: SpecWith Translations
spec = describe "createUserRenderer" $ do
  withTranslationsInLocale "en" $ do
    it "renders without bad translations" $ \runner -> do
      let input =
            CreateUser
              { name = InputInput NotPresent,
                email = InputInput NotPresent,
                confirmEmail = InputInput NotPresent,
                password = InputInput NotPresent,
                confirmPassword = InputInput NotPresent
              }
          errs = mempty
          renderAct :: HtmlT (TranslationT Identity) ()
          renderAct = renderFormT createUserRenderer input errs
          soup = runTranslationToSoup runner renderAct
      assertHasNoBadTranslations soup

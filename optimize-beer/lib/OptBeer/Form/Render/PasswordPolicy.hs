{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Form.Render.PasswordPolicy where

import Data.Password.Validate qualified as PassV
import Data.Text (Text)
import Lucid
import Noided.Web.Html.FormRender
import Noided.Translate
import GHC.Exts (IsList(..))
import Noided.Web.Html (renderTranslated)

-- | Render a password policy as a list of requirements.
renderPasswordPolicy :: (FetchMessages m, FetchHtmlFormatters m) => PassV.ValidPasswordPolicy -> HtmlT m ()
renderPasswordPolicy vpol = do
  let pol = PassV.fromValidPasswordPolicy vpol
  ul_ [class_ "password-policy-requirements"] $ do
    renderReq "min_length" (PassV.minimumLength pol)
    renderReq "max_length" (PassV.maximumLength pol)
    renderReq "uppercase" (PassV.uppercaseChars pol)
    renderReq "lowercase" (PassV.lowercaseChars pol)
    renderReq "digits" (PassV.digitChars pol)
    renderReq "special" (PassV.specialChars pol)
  where
    renderReq :: (FetchMessages m, FetchHtmlFormatters m) => Text -> Int -> HtmlT m ()
    renderReq _ 0 = return ()
    renderReq key val = li_ [class_ "password-policy-requirement"] $
      renderTranslated ["password_policy" <> textToMessageKey key] (fromList [("count", ParamInt (toInteger val))])

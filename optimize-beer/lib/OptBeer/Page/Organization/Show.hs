{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Page.Organization.Show where

import Lucid
import Noided.Web.Html (renderTranslated)
import OptBeer.DB.Table.Organization
import OptBeer.Page.Type

showOrganizationPage :: Organization -> HtmlT Page ()
showOrganizationPage org =
  div_ [class_ "organization-show-container"] $ do
    h1_ [class_ "organization-name"] $ toHtml org.name
    p_ [class_ "organization-id"] $ do
      renderTranslated ["organization.idLabel"] mempty
      " "
      toHtml (show org.id)

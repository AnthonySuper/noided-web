{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Page.Organization.Show where

import Lucid
import Noided.Pathname (usePathTemplate)
import Noided.Web.Html (renderTranslated)
import OptBeer.DB.Table.Organization
import OptBeer.Page.Type
import OptBeer.Routes (newItemPath)
import OptBeer.Type.OrganizationIdent (OrganizationIdent (..))

showOrganizationPage :: Organization -> HtmlT Page ()
showOrganizationPage org =
  div_ [class_ "organization-show-container"] $ do
    h1_ [class_ "organization-name"] $ toHtml org.name
    
    div_ [class_ "quick-actions-bar"] $ do
      a_ [class_ "button", href_ (usePathTemplate newItemPath (OrganizationById org.id))] $
        renderTranslated ["organization.items.create.link"] mempty

    p_ [class_ "organization-id"] $ do
      renderTranslated ["organization.idLabel"] mempty
      " "
      toHtml (show org.id)

    div_ [class_ "organization-items-section"] $ do
      h2_ $ renderTranslated ["organization.items.title"] mempty
      -- Items list will go here later

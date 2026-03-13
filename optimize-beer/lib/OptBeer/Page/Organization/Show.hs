{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Page.Organization.Show where

import Lucid
import Noided.Pathname (usePathTemplate)
import Noided.Translate (MessageKey)
import Noided.Web.Html (renderTranslated)
import Noided.Web.Html.FormRender (renderFormT)
import OptBeer.DB.Table.Organization
import OptBeer.Form.Render.CreateItem (createItemRenderer)
import OptBeer.Form.Type.CreateItem (emptyCreateItemForm)
import OptBeer.Page.Type
import OptBeer.Routes (createItemPath)
import OptBeer.Type.OrganizationIdent (OrganizationIdent (..))

showOrganizationPage :: Organization -> HtmlT Page ()
showOrganizationPage org =
  div_ [class_ "organization-show-container"] $ do
    h1_ [class_ "organization-name"] $ toHtml org.name
    p_ [class_ "organization-id"] $ do
      renderTranslated ["organization.idLabel"] mempty
      " "
      toHtml (show org.id)

    div_ [class_ "organization-items-section"] $ do
      h2_ $ renderTranslated ["organization.items.title"] mempty
      
      -- For now, just render the "Add Item" form
      div_ [class_ "create-item-form-container"] $ do
        h3_ $ renderTranslated ["organization.items.create.title"] mempty
        form_ [method_ "post", action_ (usePathTemplate createItemPath (OrganizationById org.id)), class_ "form"] $ do
          renderFormT createItemRenderer emptyCreateItemForm mempty
          div_ [class_ "form-buttons"] $
            button_ [class_ "button", type_ "submit"] $
              renderTranslated (["organization.items.create.button"] :: [MessageKey]) mempty

{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Page.Item.New where

import Lucid
import Noided.Form.HKD
import Noided.Pathname (usePathTemplate)
import Noided.Translate (MessageKey)
import Noided.Web.Html (renderTranslated)
import Noided.Web.Html.FormRender (renderFormT)
import OptBeer.DB.Table.Organization
import OptBeer.Form.Render.CreateItem (createItemRenderer)
import OptBeer.Form.Type.CreateItem (CreateItemF)
import OptBeer.Page.Type
import OptBeer.Routes (createItemPath)
import OptBeer.Type.OrganizationIdent (OrganizationIdent (..))

newItemPage :: Organization -> CreateItemF FormInput -> FormErrors (SubformField CreateItemF) -> HtmlT Page ()
newItemPage org input errs =
  div_ [class_ "create-item-container"] $ do
    h1_ $ renderTranslated ["organization.items.create.title"] mempty
    
    form_ [method_ "post", action_ (usePathTemplate createItemPath (OrganizationById org.id)), class_ "form", data_ "framelike" "true"] $ do
      renderFormT createItemRenderer input errs
      div_ [class_ "form-buttons"] $
        button_ [class_ "button", type_ "submit"] $
          renderTranslated (["organization.items.create.button"] :: [MessageKey]) mempty

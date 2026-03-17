{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module OptBeer.Page.Item
  ( itemsIndexPage,
    showItemPage,
    itemFormPage,
    itemFormWrapper,
    itemFormInternals,
  )
where

import Data.Foldable (forM_)
import Data.Text (Text)
import Lucid
import Noided.Form.HKD
import Noided.Pathname (usePathTemplate)
import Noided.Translate (MessageKey)
import Noided.Web.Html (FetchHtmlFormatters, FetchMessages, renderTranslated)
import Noided.Web.Html.FormRender (renderFormT)
import OptBeer.DB.Table.Item (Item, ItemF (..))
import OptBeer.DB.Table.Organization (Organization, OrganizationF (..))
import OptBeer.Form.Render.Item (itemRenderer)
import OptBeer.Form.Render.Search (searchRenderer)
import OptBeer.Form.Type.Item (ItemFormF)
import OptBeer.Form.Type.Search (SearchFormF)
import OptBeer.Render.Heading
import OptBeer.Routes (editItemPath, itemsPath, newItemPath, showItemPath)
import OptBeer.Type.OrganizationIdent (OrganizationIdent (..))

itemsIndexPage ::
  forall m t.
  (FetchMessages m, FetchHtmlFormatters m, Monad m, Foldable t) =>
  Organization ->
  SearchFormF FormInput ->
  t Item ->
  HtmlT m ()
itemsIndexPage org searchInput items =
  div_ [class_ "items-index-container"] $ do
    renderHeadingOrganization org $
      (emptyHeadingCfg @m)
        { title = renderTranslated ["organization.items.index.title"] mempty,
          actions =
            a_ [href_ (usePathTemplate newItemPath (OrganizationById org.id)), class_ "button"] $
              renderTranslated ["organization.items.index.new_button"] mempty
        }

    form_ [method_ "get", action_ (usePathTemplate itemsPath (OrganizationById org.id))] $
      renderFormT searchRenderer searchInput mempty

    table_ [class_ "pretty-table"] $ do
      thead_ $ tr_ $ do
        th_ $ renderTranslated ["form.attributes.name.name"] mempty
        th_ $ renderTranslated ["form.Item.attributes.description.name"] mempty
        th_ $ renderTranslated ["form.Item.attributes.defaultUnit.name"] mempty
        th_ "" -- Actions
      tbody_ $ forM_ items $ \item -> tr_ $ do
        td_ $ a_ [href_ (usePathTemplate showItemPath item.id)] (toHtml item.name)
        td_ $ toHtml item.description
        td_ $ toHtml (show item.defaultUnit)
        td_ $ a_ [href_ (usePathTemplate showItemPath item.id)] $
          renderTranslated ["organization.items.index.view_link"] mempty

showItemPage :: forall m. (FetchMessages m, FetchHtmlFormatters m, Monad m) => Organization -> Item -> HtmlT m ()
showItemPage org item =
  div_ [class_ "item-show-container"] $ do
    renderHeadingOrganization org $
      (emptyHeadingCfg @m)
        { breadcrumbs =
            [ a_ [href_ (usePathTemplate itemsPath (OrganizationById org.id))] $
                renderTranslated ["organization.items.index.title"] mempty
            ],
          title = toHtml item.name,
          actions =
            a_ [href_ (usePathTemplate editItemPath item.id), class_ "button"] $
              renderTranslated ["organization.items.edit.link"] mempty
        }
    
    div_ [class_ "details-card"] $ do
      div_ [class_ "detail-group"] $ do
        span_ [class_ "detail-label"] $ renderTranslated ["form.Item.attributes.description.name"] mempty
        div_ [class_ "detail-value"] (toHtml item.description)

      div_ [class_ "detail-group"] $ do
        span_ [class_ "detail-label"] $ renderTranslated ["form.Item.attributes.defaultUnit.name"] mempty
        div_ [class_ "detail-value"] (toHtml (show item.defaultUnit))

itemFormPage :: forall m. (FetchMessages m, FetchHtmlFormatters m, Monad m) => Organization -> [MessageKey] -> [MessageKey] -> Text -> ItemFormF FormInput -> FormErrors (SubformField ItemFormF) -> HtmlT m ()
itemFormPage org titleKeys buttonKeys formAction input errs =
  itemFormWrapper org titleKeys $
    itemFormInternals buttonKeys formAction input errs

itemFormWrapper :: forall m. (FetchMessages m, FetchHtmlFormatters m, Monad m) => Organization -> [MessageKey] -> HtmlT m () -> HtmlT m ()
itemFormWrapper org titleKeys inner =
  div_ [class_ "item-form-container"] $ do
    renderHeadingOrganization org $
      (emptyHeadingCfg @m)
        { breadcrumbs =
            [ a_ [href_ (usePathTemplate itemsPath (OrganizationById org.id))] $
                renderTranslated ["organization.items.index.title"] mempty
            ],
          title = renderTranslated titleKeys mempty
        }
    inner

itemFormInternals :: forall m. (FetchMessages m, FetchHtmlFormatters m, Monad m) => [MessageKey] -> Text -> ItemFormF FormInput -> FormErrors (SubformField ItemFormF) -> HtmlT m ()
itemFormInternals buttonKeys formAction input errs =
  form_ [method_ "post", action_ formAction, class_ "form", data_ "framelike" "true"] $ do
    renderFormT itemRenderer input errs
    div_ [class_ "form-buttons"] $
      button_ [class_ "button", type_ "submit"] $
        renderTranslated buttonKeys mempty

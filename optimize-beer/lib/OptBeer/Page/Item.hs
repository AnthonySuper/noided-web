{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module OptBeer.Page.Item
  ( itemsIndexPage,
    showItemPage,
  )
where

import Data.Foldable (forM_)
import Lucid
import Noided.Pathname (usePathTemplate)
import Noided.Web.Html (FetchHtmlFormatters, FetchMessages, renderTranslated)
import OptBeer.DB.Table.Item (Item, ItemF (..))
import OptBeer.DB.Table.Organization (Organization, OrganizationF (..))
import OptBeer.Page.Type
import OptBeer.Routes (editItemPath, itemsPath, newItemPath, showItemPath)
import OptBeer.Type.OrganizationIdent (OrganizationIdent (..))

itemsIndexPage :: (FetchMessages m, FetchHtmlFormatters m, Monad m, Foldable t) => Organization -> t Item -> HtmlT m ()
itemsIndexPage org items =
  div_ [class_ "items-index-container"] $ do
    div_ [class_ "header-with-actions"] $ do
      h1_ $ renderTranslated ["organization.items.index.title"] mempty
      a_ [href_ (usePathTemplate newItemPath (OrganizationById org.id)), class_ "button"] $
        renderTranslated ["organization.items.index.new_button"] mempty
    
    table_ [class_ "table items-table"] $ do
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

showItemPage :: (FetchMessages m, FetchHtmlFormatters m, Monad m) => Organization -> Item -> HtmlT m ()
showItemPage org item =
  div_ [class_ "item-show-container"] $ do
    nav_ [class_ "breadcrumb"] $ do
      a_ [href_ (usePathTemplate itemsPath (OrganizationById org.id))] $
        renderTranslated ["organization.items.index.title"] mempty
      span_ " / "
      span_ (toHtml item.name)

    div_ [class_ "header-with-actions"] $ do
      h1_ (toHtml item.name)
      a_ [href_ (usePathTemplate editItemPath item.id), class_ "button"] $
        renderTranslated ["organization.items.edit.link"] mempty
    
    div_ [class_ "item-details"] $ do
      div_ [class_ "detail-group"] $ do
        span_ [class_ "detail-label"] $ renderTranslated ["form.Item.attributes.description.name"] mempty
        div_ [class_ "detail-value"] (toHtml item.description)

      div_ [class_ "detail-group"] $ do
        span_ [class_ "detail-label"] $ renderTranslated ["form.Item.attributes.defaultUnit.name"] mempty
        div_ [class_ "detail-value"] (toHtml (show item.defaultUnit))

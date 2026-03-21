{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module OptBeer.Page.Recipe
  ( recipesIndexPage,
    recipeFormPage,
    recipeFormWrapper,
    recipeFormInternals,
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
import OptBeer.DB.Table.Recipe (Recipe, RecipeF (..))
import OptBeer.DB.Table.Organization (Organization, OrganizationF (..))
import OptBeer.Form.Render.Recipe (recipeRenderer)
import OptBeer.Form.Render.Search (searchRenderer)
import OptBeer.Form.Type.Recipe (RecipeFormF)
import OptBeer.Form.Type.Search (SearchFormF)
import OptBeer.Render.Heading
import OptBeer.Routes (newRecipePath, recipesPath, showRecipePath)
import OptBeer.Type.OrganizationIdent (OrganizationIdent (..))

recipesIndexPage ::
  forall m t.
  (FetchMessages m, FetchHtmlFormatters m, Monad m, Foldable t) =>
  Organization ->
  SearchFormF FormInput ->
  t Recipe ->
  HtmlT m ()
recipesIndexPage org searchInput recipes =
  div_ [class_ "recipes-index-container"] $ do
    renderHeadingOrganization org $
      (emptyHeadingCfg @m)
        { title = renderTranslated ["organization.recipes.index.title"] mempty,
          actions =
            a_ [href_ (usePathTemplate newRecipePath (OrganizationById org.id)), class_ "button"] $
              renderTranslated ["organization.recipes.index.new_button"] mempty
        }

    form_ [method_ "get", action_ (usePathTemplate recipesPath (OrganizationById org.id))] $
      renderFormT searchRenderer searchInput mempty

    table_ [class_ "pretty-table"] $ do
      thead_ $ tr_ $ do
        th_ $ renderTranslated ["form.attributes.name.name"] mempty
        th_ $ renderTranslated ["form.attributes.description.name"] mempty
        th_ $ renderTranslated ["form.Recipe.attributes.batchSize.name"] mempty
        th_ $ renderTranslated ["organization.recipes.index.actions_header"] mempty
      tbody_ $ forM_ recipes $ \recipe -> tr_ $ do
        td_ $ a_ [href_ (usePathTemplate showRecipePath recipe.id)] (toHtml recipe.name)
        td_ $ toHtml recipe.description
        td_ $ toHtml (show recipe.batchSize) <> " " <> toHtml (show recipe.batchSizeUnit)
        td_ $ a_ [href_ (usePathTemplate showRecipePath recipe.id)] $
          renderTranslated ["organization.recipes.index.view_link"] mempty

recipeFormPage :: forall m. (FetchMessages m, FetchHtmlFormatters m, Monad m) => Organization -> [MessageKey] -> [MessageKey] -> Text -> RecipeFormF FormInput -> FormErrors (SubformField RecipeFormF) -> HtmlT m ()
recipeFormPage org titleKeys buttonKeys formAction input errs =
  recipeFormWrapper org titleKeys $
    recipeFormInternals buttonKeys formAction input errs

recipeFormWrapper :: forall m. (FetchMessages m, FetchHtmlFormatters m, Monad m) => Organization -> [MessageKey] -> HtmlT m () -> HtmlT m ()
recipeFormWrapper org titleKeys inner =
  div_ [class_ "recipe-form-container"] $ do
    renderHeadingOrganization org $
      (emptyHeadingCfg @m)
        { breadcrumbs =
            [ a_ [href_ (usePathTemplate recipesPath (OrganizationById org.id))] $
                renderTranslated ["organization.recipes.index.title"] mempty
            ],
          title = renderTranslated titleKeys mempty
        }
    inner

recipeFormInternals :: forall m. (FetchMessages m, FetchHtmlFormatters m, Monad m) => [MessageKey] -> Text -> RecipeFormF FormInput -> FormErrors (SubformField RecipeFormF) -> HtmlT m ()
recipeFormInternals buttonKeys formAction input errs =
  form_ [method_ "post", action_ formAction, class_ "form", data_ "framelike" "true"] $ do
    renderFormT recipeRenderer input errs
    div_ [class_ "form-buttons"] $
      button_ [class_ "button", type_ "submit"] $
        renderTranslated buttonKeys mempty

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module OptBeer.Page.Recipe
  ( recipeFormPage,
    recipeFormWrapper,
    recipeFormInternals,
  )
where

import Data.Text (Text)
import Lucid
import Noided.Form.HKD
import Noided.Pathname (usePathTemplate)
import Noided.Translate (MessageKey)
import Noided.Web.Html (FetchHtmlFormatters, FetchMessages, renderTranslated)
import Noided.Web.Html.FormRender (renderFormT)
import OptBeer.DB.Table.Organization (Organization, OrganizationF (..))
import OptBeer.Form.Render.Recipe (recipeRenderer)
import OptBeer.Form.Type.Recipe (RecipeFormF)
import OptBeer.Render.Heading
import OptBeer.Routes (recipesPath)
import OptBeer.Type.OrganizationIdent (OrganizationIdent (..))

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

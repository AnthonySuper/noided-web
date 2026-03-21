{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Form.Render.Recipe where

import Data.Scientific (Scientific)
import Data.Text (Text, pack)
import Lucid
import Noided.Form.HKD
import Noided.Web.Html.FormRender
import OptBeer.Form.Render.Base
import OptBeer.Form.Type.Recipe

recipeRenderer ::
  ( FetchMessages m,
    FetchHtmlFormatters m
  ) =>
  HtmlFormRendererT m (SubformField RecipeFormF)
recipeRenderer =
  fieldWrapModelName "Recipe" $
    wrapField baseErrorWrapper (subformField recipeRendererT)

recipeRendererT ::
  (FetchMessages m, FetchHtmlFormatters m) =>
  RecipeFormF (FormRenderer (HtmlFormT m))
recipeRendererT =
  RecipeForm
    { name = textField,
      description = textField,
      batchSize = scientificField,
      batchSizeUnit = unitSelectField,
      targetOg = scientificField,
      targetFg = scientificField,
      targetAbv = scientificField,
      targetIbu = formField $ fieldWrapper $ renderInputTag [class_ "form-field-input", type_ "number"],
      targetSrm = scientificField,
      boilTimeMinutes = formField $ fieldWrapper $ renderInputTag [class_ "form-field-input", type_ "number"],
      targetEfficiency = scientificField
    }
  where
    textField = formField $ fieldWrapper $ renderInputTag [class_ "form-field-input", type_ "text"]
    scientificField = formField $ fieldWrapper $ renderInputTag' scientificToText [class_ "form-field-input", type_ "number", step_ "any"]
    unitSelectField = formField $ fieldWrapper $ renderEnumSelectTag [class_ "form-field-input"]

    scientificToText :: Scientific -> Text
    scientificToText = pack . show . (realToFrac @Scientific @Double)

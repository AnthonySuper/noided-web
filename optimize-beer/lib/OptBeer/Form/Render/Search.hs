{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Form.Render.Search where

import Lucid
import Noided.Form.HKD
import Noided.Web.Html.FormRender
import OptBeer.Form.Type.Pagination
import OptBeer.Form.Type.Search

-- | A renderer for the search form.
-- This only renders the 'search' field.
-- 'pagination' is expected to be handled by the URL or hidden.
searchRenderer ::
  ( FetchMessages m,
    FetchHtmlFormatters m
  ) =>
  HtmlFormRendererT m (SubformField SearchFormF)
searchRenderer =
  fieldWrapModelName "Search" $
    subformField searchRendererT

searchRendererT ::
  (FetchMessages m, FetchHtmlFormatters m) =>
  SearchFormF (FormRenderer (HtmlFormT m))
searchRendererT =
  SearchForm
    { search = formField $ fieldWrapper $ do
        renderInputTag [class_ "search-form-input", type_ "search"]
        button_ [type_ "submit", class_ "search-form-button", title_ "Search"] $
          span_ [class_ "search-icon"] ""
      , pagination = subformField $ PaginationForm
        { page = renderInput $ const $ return (),
          perPage = renderInput $ const $ return ()
        }
    }
  where
    fieldWrapper inputAct = div_ [class_ "search-form-wrapper"] $ do
      renderLabelTag [class_ "search-form-label sr-only"]
      div_ [class_ "search-form-group"] $ do
        _ <- inputAct
        return ()
      ul_ [class_ "form-field-errors"] $
        renderFieldErrors (li_ [class_ "form-field-error"])

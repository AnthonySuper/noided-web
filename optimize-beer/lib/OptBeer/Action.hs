{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Action (optBeerActions) where

import Data.Function
import Data.Functor.Identity
import Effectful
import Noided.Web.Effect
import Noided.Web.Internal.Type.PageAction
import OptBeer.Action.User (userActions)
import OptBeer.Page.Layout (pageLayout)
import OptBeer.Page.Type (mapResponsesToPage)

optBeerActions :: (FetchMessagesE :> es) => PageRoutes Identity (Eff es)
optBeerActions =
  beforeTransform
    & pagesAddLayout pageLayout
    & mapResponsesToPage
    & pagesAroundAction (runFetchHtmlFormattersE mempty)
  where
    beforeTransform =
      userActions

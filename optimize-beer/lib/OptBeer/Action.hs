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
import Noided.Web.Internal.Type.ServerEnv

optBeerActions :: (FetchMessagesE :> es, GetServerEnv :> es, IOE :> es) => PageRoutes Identity (Eff es)
optBeerActions =
  beforeTransform
    & pagesAddLayout pageLayout
    & mapResponsesToPage
    & pagesAroundAction runFrontendAssets
    & pagesAroundAction (runFetchHtmlFormattersE mempty)
  where
    runFrontendAssets act = do
      env <- getServerEnv
      case env of
        Development -> runFrontendAssetsDev "http://localhost:5173" act
        _ -> do
          res <- loadViteManifest "static/.vite/manifest.json"
          case res of
            Left _ -> runFrontendAssetsProd "/static/" (ViteManifest mempty) act
            Right m -> runFrontendAssetsProd "/static/" m act
    beforeTransform =
      userActions

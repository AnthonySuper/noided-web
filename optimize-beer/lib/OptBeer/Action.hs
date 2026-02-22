{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Action (optBeerActions) where

import Data.Function
import Data.Functor.Identity
import Data.Text
import Effectful
import Noided.Sql (SessionError)
import Noided.Web.Effect
import Noided.Web.PageAction
import OptBeer.Action.Home (homeActions)
import OptBeer.Action.Session (sessionActions)
import OptBeer.Action.User (userActions)
import OptBeer.Effect.CurrentActor
import OptBeer.Effect.HashPassword
import Noided.Web.Error
import OptBeer.Page.Error
import OptBeer.Page.Layout (pageLayout)
import OptBeer.Page.Type

optBeerActions ::
  ( FetchMessagesE :> es,
    Log :> es,
    GetServerEnv :> es,
    GetRequestBody :> es,
    GetRemoteIp :> es,
    GetHeaders :> es,
    GetCookies :> es,
    RunTransaction :> es,
    CurrentTime :> es,
    Signing :> es,
    WriteHeader :> es,
    IOE :> es
  ) =>
  PageRoutes Identity (Eff es)
optBeerActions =
  beforeTransform
    & pagesAddLayout pageLayout
    & pagesHandleError handleBadRequest
    & pagesHandleError handleUnauthorized
    & pagesHandleError handleForbidden
    & pagesHandleError handleNotFound
    & pagesHandleError handleConflict
    & pagesHandleError handleIAmATeapot
    & pagesHandleError handleTooManyRequests
    & pagesHandleError handleUnavailableForLegalReasons
    & pagesAroundAction runSettingCookies
    & pagesHandleError handleSessionError

    & mapResponsesToPage
    & pagesAroundAction runWithCurrentActorFromSession
    & pagesAroundAction runFrontendAssets
    & pagesAroundAction (runFetchHtmlFormattersE mempty)
  where
    handleBadRequest cs (BadRequest msg) = respondBadRequest cs msg
    handleUnauthorized cs (Unauthorized msg) = respondUnauthorized cs msg
    handleForbidden cs (Forbidden msg) = respondForbidden cs msg
    handleNotFound cs (NotFound msg) = respondNotFound cs msg
    handleConflict cs (Conflict msg) = respondConflict cs msg
    handleIAmATeapot cs (IAmATeapot msg) = respondIAmATeapot cs msg
    handleTooManyRequests cs (TooManyRequests msg) = respondTooManyRequests cs msg
    handleUnavailableForLegalReasons cs (UnavailableForLegalReasons msg) = respondUnavailableForLegalReasons cs msg
    handleSessionError cs (err :: SessionError) = respondInternalError cs (pack $ show err)

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
      pagesAroundAction runHashPasswordBCrypt $
        userActions
          <> sessionActions
          <> homeActions

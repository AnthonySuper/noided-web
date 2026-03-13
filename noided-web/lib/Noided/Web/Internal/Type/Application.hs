{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Type.Application where

import Data.Dependent.Map qualified as DMap
import Data.Functor.Identity
import Data.Monoid (Last (..))
import Effectful
import Effectful.Error.Static
import GHC.Generics
import Network.Wai qualified as Wai
import Noided.Server.Internal.Type.Request
import Noided.Server.Internal.Type.Server
import Noided.Server.Internal.Type.VerbRouter qualified as VR
import Noided.Web.Internal.Type.Endpoint
import Noided.Web.Internal.Type.ErrorRenderer
import Noided.Web.Internal.Type.PageAction
import Noided.Web.Internal.Type.Response
import Optics.Core
import Type.Reflection

-- | Configuration for application routes.
data ApplicationRouteConfig m
  = MkApplicationRouteConfig
  { pages :: PageRoutes Identity m,
    miscEndpoints :: SomeEndpoints m,
    errorHandlers :: ErrorRenderers,
    production404 :: Last (Wai.Request -> IO Wai.Response)
  }
  deriving (Generic)
  deriving (Semigroup, Monoid) via (Generically (ApplicationRouteConfig m))

-- | Build an application route config with some pages.
withPages :: PageRoutes Identity m -> ApplicationRouteConfig m
withPages p = mempty & #pages .~ p

-- | Build an application route config with some misc endpoints.
withMisc :: SomeEndpoints m -> ApplicationRouteConfig m
withMisc p = mempty & #miscEndpoints .~ p

-- | Build an application route config with some error handlers.
withErrorHandlers :: ErrorRenderers -> ApplicationRouteConfig m
withErrorHandlers p = mempty & #errorHandlers .~ p

-- | Build an application route config with a custom production 404 handler.
-- When a route is not found in production (or test) mode, this handler is called.
-- In development mode, a debug page listing all routes is shown regardless,
-- and this custom handler is not used.
withProduction404 :: (Wai.Request -> IO Wai.Response) -> ApplicationRouteConfig m
withProduction404 handler = mempty & #production404 .~ Last (Just handler)

-- | Get the production 404 handler, if one has been set.
getProduction404 :: ApplicationRouteConfig m -> Maybe (Wai.Request -> IO Wai.Response)
getProduction404 (MkApplicationRouteConfig _ _ _ p) = getLast p

-- | Map a config to an actual application, which we can do routing in.
--
-- This internally uses a 'Data.Dependent.Map' along with a 'Noided.Web.Internal.Type.VerbRouter' to
-- ensure that page routes and verb routes are modeled as much as possible.
configToApplication :: (Monad m) => ApplicationRouteConfig m -> Application m
configToApplication (MkApplicationRouteConfig pages misc errs _) =
  MkApplication
    (pagesToSomeEndpoints pages <> misc)
    errs

pagesToSomeEndpoints :: (Monad m) => PageRoutes Identity m -> SomeEndpoints m
pagesToSomeEndpoints (MkPageRoutes routes) =
  foldMap (singletonSomeEndpoints . pageToSomeEndpoint) routes

singletonSomeEndpoints :: SomeEndpoint m -> SomeEndpoints m
singletonSomeEndpoints (SomeEndpoint method pt ep) =
  MkSomeEndpoints $ DMap.singleton pt (MkVerbRouterOf $ VR.singleton method ep)

pageToSomeEndpoint :: (Monad m) => SomePageAction Identity m -> SomeEndpoint m
pageToSomeEndpoint (SomePageAction method pt act) =
  SomeEndpoint method pt (pageActionToEndpoint act)

pageActionToEndpoint :: (Monad m) => PageAction Identity m pathParams -> Endpoint m pathParams
pageActionToEndpoint (PageAct act) =
  MkEndpoint
    [ ( "text/html",
        \req -> do
          pageResp <- act req.urlParams
          let resp = runIdentity $ pageResponseToResponse FullPage pageResp
          pure (Right resp)
      ),
      ( "application/vnd.noided-fragment",
        \req -> do
          pageResp <- act req.urlParams
          let resp = runIdentity $ pageResponseToResponse Fragment pageResp
          pure (Right resp)
      ),
      ( "application/vnd.noided-fragment.form",
        \req -> do
          pageResp <- act req.urlParams
          let resp = runIdentity $ pageResponseToResponse Fragment pageResp
          pure (Right resp)
      )
    ]

-- | An application in some monad.
data Application m
  = MkApplication
  { applicationEndpoints :: SomeEndpoints m,
    errorHandlers :: ErrorRenderers
  }

applicationAroundAll ::
  (forall pathParams. EndpointAction monad pathParams -> EndpointAction monad' pathParams) ->
  Application monad ->
  Application monad'
applicationAroundAll f (MkApplication eps errs) =
  MkApplication (aroundSomeEndpointActions f eps) errs

applicationHoistM ::
  (forall a. monad a -> monad' a) ->
  Application monad ->
  Application monad'
applicationHoistM f = applicationAroundAll (\action request -> f (action request))

applicationHandleAsServerError' ::
  forall err es.
  (Typeable err) =>
  (err -> String) ->
  Application (Eff (Error err : es)) ->
  Application (Eff es)
applicationHandleAsServerError' display (MkApplication eps errs) =
  MkApplication (someEndpointsHandleAsServerError' display eps) errs

applicationHandleAsServerError ::
  (Typeable err, Show err) =>
  Application (Eff (Error err : es)) ->
  Application (Eff es)
applicationHandleAsServerError = applicationHandleAsServerError' show

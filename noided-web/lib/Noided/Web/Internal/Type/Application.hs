{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Type.Application where

import Data.Functor.Identity
import Effectful
import Effectful.Error.Static
import GHC.Generics
import Noided.Server.Internal.Type.Request
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
    errorHandlers :: ErrorRenderers
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

-- | Map a config to an actual application, which we can do routing in.
--
-- This internally uses a 'Data.Dependent.Map' along with a 'Noided.Web.Internal.Type.VerbRouter' to
-- ensure that page routes and verb routes are modeled as much as possible.
configToApplication :: (Monad m) => ApplicationRouteConfig m -> Application m
configToApplication (MkApplicationRouteConfig pages misc errs) =
  MkApplication
    (cleanupSomeEndpoints $ pagesToSomeEndpoints pages <> misc)
    errs

pagesToSomeEndpoints :: (Monad m) => PageRoutes Identity m -> SomeEndpoints m
pagesToSomeEndpoints (MkPageRoutes routes) =
  MkSomeEndpoints $ fmap pageToSomeEndpoint routes

pageToSomeEndpoint :: (Monad m) => SomePageAction Identity m -> SomeEndpoint m
pageToSomeEndpoint (SomePageAction method pt act) =
  SomeEndpoint method pt (pageActionToEndpoint act)

pageActionToEndpoint :: (Monad m) => PageAction Identity m pathParams -> Endpoint m pathParams
pageActionToEndpoint (PageAct act) =
  MkEndpoint
    [ ( "text/html",
        \req -> do
          pageResp <- act req.urlParams
          resp <- pure $ runIdentity $ pageResponseToResponse FullPage pageResp
          pure (Right resp)
      ),
      ( "application/vnd.noided-fragment",
        \req -> do
          pageResp <- act req.urlParams
          resp <- pure $ runIdentity $ pageResponseToResponse Fragment pageResp
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

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Type.Application where

import Data.Dependent.Map qualified as DMap
import Data.Foldable
import Data.Functor.Identity
import Effectful
import Effectful.Error.Static
import GHC.Generics
import Noided.Server.Internal.Type.VerbRouter
import Noided.Web.Internal.Type.Endpoint
import Noided.Web.Internal.Type.ErrorRenderer
import Noided.Web.Internal.Type.PageAction
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
configToApplication :: ApplicationRouteConfig m -> Application m
configToApplication = error "not sure"

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
applicationAroundAll _ = error "TODO: implement me"

applicationHoistM ::
  (forall a. monad a -> monad' a) ->
  Application monad ->
  Application monad'
applicationHoistM _ = error "TODO: implement me"

applicationHandleAsServerError' ::
  forall err es.
  (Typeable err) =>
  (err -> String) ->
  Application (Eff (Error err : es)) ->
  Application (Eff es)
applicationHandleAsServerError' = error "TODO: implement me"

applicationHandleAsServerError ::
  (Typeable err, Show err) =>
  Application (Eff (Error err : es)) ->
  Application (Eff es)
applicationHandleAsServerError = applicationHandleAsServerError' show

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Application
  ( -- * Application route configs
    ApplicationRouteConfig,
    withPages,
    withMisc,
    withErrorHandlers,
    configToApplication,

    -- * Applications
    Application,
    applicationAroundAll,

    -- ** Handling errors
    applicationHandleAsServerError,
    applicationHandleAsServerError',
    applicationInterpretRequest,
  )
where

import Control.Arrow
import Data.Function
import Data.Some.Newtype
import Effectful
import Network.Wai qualified as Wai
import Noided.Server.Internal.Type.Request
import Noided.Server.Internal.Type.Server
import Noided.Web.Internal.Effect.SomeRequest
import Noided.Web.Internal.Type.Application

-- | Interpret request-related actions from a request.
applicationInterpretRequest ::
  Application
    ( Eff
        ( GetHeaders
            : GetQueryParams
            : GetRequestBody
            : es
        )
    ) ->
  Application (Eff es)
applicationInterpretRequest = applicationAroundAll $ \act req ->
  act req
    & runWithHeaders req.headers
    & runWithQueryParams req.queryParams
    & runWithRequestBody req.body

-- | Transform an application into server actions, suitable for use with noided-server.
toServerActions :: Application m -> [SomeAction m Wai.Response]
toServerActions = error "TODO: implement me"

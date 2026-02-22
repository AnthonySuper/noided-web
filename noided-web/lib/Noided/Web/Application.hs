{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Application
  ( -- * Application route configs
    ApplicationRouteConfig,
    withPages,
    withMisc,
    withErrorHandlers,
    withProduction404,
    getProduction404,
    configToApplication,

    -- * Applications
    Application,
    applicationAroundAll,
    applicationHoistM,
    toServerActions,

    -- ** Handling errors
    applicationHandleAsServerError,
    applicationHandleAsServerError',

    -- ** Common application runners
    applicationInterpretCommon,
    applicationInterpretRequest,
    applicationInterpretResponse,
  )
where

import Data.ByteString.Lazy qualified as LBS
import Data.Function
import Data.Map.Strict qualified as Map
import Data.Maybe
import Effectful
import Network.HTTP.Media
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai qualified as Wai
import Noided.Server.Internal.Type.Action
import Noided.Server.Internal.Type.Request
import Noided.Web.Effect.CurrentTime
import Noided.Web.Effect.GetCookies
import Noided.Web.Effect.Signing
import Noided.Web.Effect.WriteHeader
import Noided.Web.Internal.Effect.SomeRequest
import Noided.Web.Internal.Type.Application
import Noided.Web.Internal.Type.Endpoint
import Noided.Web.Internal.Type.ErrorRenderer
import Noided.Web.Internal.Type.Response
import Optics.Core

applicationInterpretCommon ::
  (Signing :> es, CurrentTime :> es) =>
  Application
    ( Eff
        ( WriteHeader
            : GetCookies
            : GetRemoteIp
            : GetHeaders
            : GetQueryParams
            : GetRequestBody
            : es
        )
    ) ->
  Application (Eff es)
applicationInterpretCommon =
  applicationInterpretRequest . applicationInterpretResponse

-- | Interpret request-related actions from a request.
applicationInterpretRequest ::
  (Signing :> es, CurrentTime :> es) =>
  Application
    ( Eff
        ( GetCookies
            : GetRemoteIp
            : GetHeaders
            : GetQueryParams
            : GetRequestBody
            : es
        )
    ) ->
  Application (Eff es)
applicationInterpretRequest = applicationAroundAll $ \act req ->
  act req
    & runWithActualCookies
    & runWithRemoteIp req.remoteHost
    & runWithHeaders req.headers
    & runWithQueryParams req.queryParams
    & runWithRequestBody req.body

applicationInterpretResponse :: Application (Eff (WriteHeader : es)) -> Application (Eff es)
applicationInterpretResponse = applicationAroundAll $ \act req -> do
  (res, headerMap) <- runWriteHeaderMap (act req)
  return $ res & _Right % #headers %~ (<> headerMap)

-- | Transform an application into server actions, suitable for use with noided-server.
toServerActions :: (Monad m) => Application m -> [SomeAction m Wai.Response]
toServerActions (MkApplication eps errs) =
  fmap (endpointToSomeAction errs) (getSomeEndpoints eps)

endpointToSomeAction :: (Monad m) => ErrorRenderers -> SomeEndpoint m -> SomeAction m Wai.Response
endpointToSomeAction errs (SomeEndpoint method pt (MkEndpoint routes)) =
  SomeAction
    method
    pt
    ( Act $ \req -> do
        let acceptHeader = fromMaybe "*/*" $ Map.lookup hAccept req.headers
        let mSelected = mapAccept routes acceptHeader
        case mSelected of
          Nothing -> pure $ Wai.responseLBS status406 [] "Not Acceptable"
          Just action -> do
            res <- action req
            case res of
              Left sse -> pure $ responseToWai $ useErrorRenderersDebug errs sse acceptHeader
              Right resp -> pure $ responseToWai resp
    )

responseToWai :: Response -> Wai.Response
responseToWai (Response status headers body) =
  case body of
    ByteStringBody bs -> Wai.responseLBS status headers (LBS.fromStrict bs)
    LazyByteStringBody lbs -> Wai.responseLBS status headers lbs
    BuilderBody b -> Wai.responseBuilder status headers b

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web (useNoidedApplication) where

import Control.Arrow
import Control.Exception (throwIO)
import Data.Function
import Effectful
import Network.Wai qualified as Wai
import Noided.Server
import Noided.Web.Application
import Noided.Web.ApplicationConfig
import Noided.Web.Effect
import Noided.Web.Html.Internal.Class.FetchMessages
import Noided.Web.Internal.Type.ServerEnv

useNoidedApplication ::
  ApplicationRouteConfig
    ( Eff
        [ RunTransaction,
          FetchMessagesE,
          TimeEvent,
          WriteHeader,
          GetHeaders,
          GetQueryParams,
          GetRequestBody,
          HasTranslations,
          UseConnection,
          Log,
          CurrentTime,
          IOE
        ]
    ) ->
  (Wai.Request -> IO Wai.Response) ->
  (Wai.Application -> IO b) ->
  IO b
useNoidedApplication appConfig on404 useApplication = do
  let application = configToApplication appConfig
  config <- readConfiguration >>= either throwIO pure
  withInterpretersFromConfig config $ \interpret -> do
    let allRan =
          application
            & applicationHoistM
              ( runRunTransactionFromConnection
                  >>> withMessagesFromQueryParams "en"
                  >>> ( case config.serverEnv of
                          Production -> runIgnoringEventTimings
                          _ -> runLoggingEventTimingsToHeader
                      )
              )
            & applicationInterpretCommon
            & applicationHoistM
              ( interpret
                  >>> runCurrentTime
                  >>> runEff
              )
    let server = makeServer (toServerActions allRan) on404
    useApplication (toWaiApplication server)

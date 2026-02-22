{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web
  ( useNoidedApplication,

    -- * Application Configuration and Types
    module Noided.Web.Application,
    module Noided.Web.ApplicationConfig,

    -- * Page Actions and Routes
    module Noided.Web.PageAction,

    -- * Responses
    module Noided.Web.Response,

    -- * Effects
    module Noided.Web.Effect,
  )
where

import Control.Arrow
import Control.Exception (throwIO)
import Data.Function
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import Lucid
import Network.HTTP.Types.Status (status404)
import Network.Wai qualified as Wai
import Noided.Pathname
import Noided.Server
import Noided.Server.Internal.Type.Server (someActionsToRouter)
import Noided.Web.Application
import Noided.Web.ApplicationConfig
import Noided.Web.Effect
import Noided.Web.PageAction
import Noided.Web.Response

useNoidedApplication ::
  ApplicationRouteConfig
    ( Eff
        [ GetServerEnv,
          RunTransaction,
          FetchMessagesE,
          TimeEvent,
          WriteHeader,
          GetCookies,
          GetRemoteIp,
          GetHeaders,
          GetQueryParams,
          GetRequestBody,
          Signing,
          HasTranslations,
          UseConnection,
          Log,
          CurrentTime,
          IOE
        ]
    ) ->
  (Wai.Application -> IO b) ->
  IO b
useNoidedApplication appConfig useApplication = do
  let application = configToApplication appConfig
  config <- readConfiguration >>= either throwIO pure
  withInterpretersFromConfig config $ \interpret -> do
    let allRan =
          application
            & applicationHoistM
              ( runServerEnv config.serverEnv
                  >>> runRunTransactionFromConnection
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
    let actions = toServerActions allRan
    let on404 = case config.serverEnv of
          Development -> developmentNotFound actions
          _ -> fromMaybe defaultNotFound (getProduction404 appConfig)
    let server = makeServer actions on404
    useApplication (toWaiApplication server)

-- | Default 404 handler for non-development modes (production and test).
-- Displays a minimal "Not Found" page.
defaultNotFound :: Wai.Request -> IO Wai.Response
defaultNotFound _ =
  pure $
    Wai.responseLBS
      status404
      [("Content-Type", "text/html; charset=utf-8")]
      "<html><head><title>Not Found</title></head><body><h1>Not Found</h1><p>The requested page could not be found.</p></body></html>"

-- | Development 404 handler.
-- Displays a debug page listing all known routes and why each failed to match.
developmentNotFound :: [SomeAction IO Wai.Response] -> Wai.Request -> IO Wai.Response
developmentNotFound actions req = do
  let router = someActionsToRouter actions
  let pathPieces = Wai.pathInfo req
  let results = testUrlResult pathPieces router
  pure $
    Wai.responseLBS
      status404
      [("Content-Type", "text/html; charset=utf-8")]
      (renderBS $ developmentNotFoundPage pathPieces results)

developmentNotFoundPage :: [Text] -> [TemplateMatchResult contained] -> Html ()
developmentNotFoundPage pathPieces results = do
  doctype_
  html_ $ do
    head_ $ do
      title_ "404 Not Found (Development)"
      style_ pageStyle
    body_ $ do
      h1_ "404 Not Found"
      p_ $ do
        "No route matched "
        code_ $ toHtml ("/" <> Text.intercalate "/" pathPieces)
      h2_ "Available Routes"
      if null results
        then p_ [class_ "muted"] "No routes are registered."
        else ul_ [class_ "routes"] $ mapM_ renderResult results
  where
    renderResult :: TemplateMatchResult contained -> Html ()
    renderResult (TemplateMatch template _ matchResult) =
      li_ $ do
        code_ $ toHtml (show template)
        " — "
        case matchResult of
          Right _ ->
            span_ [class_ "match"] "path matched (method may not be supported)"
          Left (ExtraPieces extra) -> do
            span_ [class_ "fail"] "too many path segments: "
            code_ $ toHtml (Text.intercalate "/" extra)
          Left (MatchFailedAfter matched msg) -> do
            span_ [class_ "fail"] "failed"
            case matched of
              [] -> mempty
              _ -> do
                " after "
                code_ $ toHtml (Text.intercalate "/" matched)
            ": "
            renderFailureMessage msg

    renderFailureMessage :: TemplateMatchFailureMessage -> Html ()
    renderFailureMessage (CaptureFailed tr got _) = do
      "could not parse "
      code_ $ toHtml (show tr)
      " from "
      code_ $ toHtml got
    renderFailureMessage (StaticFailed got expected) = do
      code_ $ toHtml got
      " did not match expected "
      code_ $ toHtml expected
    renderFailureMessage NotEnough =
      "ran out of path segments"

    pageStyle :: Text
    pageStyle =
      "body{font-family:sans-serif;padding:2em;max-width:900px;margin:0 auto}"
        <> "h1{color:#c00}"
        <> "code{background:#f0f0f0;padding:2px 4px;border-radius:3px;font-size:.9em}"
        <> ".routes{line-height:2}"
        <> ".match{color:green}"
        <> ".fail{color:#c00}"
        <> ".muted{color:#999}"

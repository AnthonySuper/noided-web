{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Page.Error where

import Control.Monad.Reader (ask)
import Data.Text (Text)
import GHC.Stack
import Lucid
import Network.HTTP.Types.Status
import Noided.Web.Effect (ServerEnv (..))
import Noided.Web.Response
import OptBeer.Page.Type

-- | Render a callstack nicely using HTML elements and CSS grid.
renderCallStack :: (Monad m) => CallStack -> HtmlT m ()
renderCallStack cs = do
  div_ [class_ "callstack-container"] $ do
    h3_ [class_ "callstack-header"] "CallStack"
    div_ [class_ "callstack-grid"] $
      mapM_ renderEntry (getCallStack cs)
  where
    renderEntry (name, loc) = div_ [class_ "callstack-row"] $ do
      _ <- div_ [class_ "callstack-cell callstack-fn"] (toHtml name)
      div_ [class_ "callstack-cell callstack-loc"] $ do
        span_ [class_ "loc-module"] (toHtml $ srcLocModule loc)
        " ("
        span_ [class_ "loc-file"] (toHtml $ srcLocFile loc)
        ":"
        toHtml (show $ srcLocStartLine loc)
        ":"
        toHtml (show $ srcLocStartCol loc)
        ")"

-- | A generic error page that shows a callstack in development.
errorPage :: CallStack -> Text -> HtmlT Page ()
errorPage cs msg = do
  env <- ask
  div_ [class_ "error-container"] $ do
    case env.serverEnv of
      Development -> do
        h1_ [class_ "error-title"] "Server Error"
        div_ [class_ "error-msg"] $ toHtml msg
        renderCallStack cs
      _ -> div_ [class_ "production-container"] $ do
        h1_ "Something went wrong"
        p_ "An unexpected error occurred. Our team has been notified. Please try again later."

-- | Create a response with the error page.
respondError :: Status -> CallStack -> Text -> PageResponse Page
respondError status cs msg = respondPage status (errorPage cs msg)

-- | Create a 400 Bad Request response with the error page.
respondBadRequest :: CallStack -> Text -> PageResponse Page
respondBadRequest cs msg = respondError badRequest400 cs msg

-- | Create a 500 Internal Server Error response with the error page.
respondInternalError :: CallStack -> Text -> PageResponse Page
respondInternalError cs msg = respondError internalServerError500 cs msg

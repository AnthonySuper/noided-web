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
respondBadRequest = respondError badRequest400

-- | Create a 401 Unauthorized response with the error page.
respondUnauthorized :: CallStack -> Text -> PageResponse Page
respondUnauthorized = respondError unauthorized401

-- | Create a 403 Forbidden response with the error page.
respondForbidden :: CallStack -> Text -> PageResponse Page
respondForbidden = respondError forbidden403

-- | Create a 404 Not Found response with the error page.
respondNotFound :: CallStack -> Text -> PageResponse Page
respondNotFound = respondError notFound404

-- | Create a 409 Conflict response with the error page.
respondConflict :: CallStack -> Text -> PageResponse Page
respondConflict = respondError conflict409

-- | Create a 418 I Am A Teapot response with the error page.
respondIAmATeapot :: CallStack -> Text -> PageResponse Page
respondIAmATeapot = respondError imATeapot418

-- | Create a 429 Too Many Requests response with the error page.
respondTooManyRequests :: CallStack -> Text -> PageResponse Page
respondTooManyRequests = respondError tooManyRequests429

-- | Create a 451 Unavailable For Legal Reasons response with the error page.
respondUnavailableForLegalReasons :: CallStack -> Text -> PageResponse Page
respondUnavailableForLegalReasons = respondError unavailableForLegalReasons451

unavailableForLegalReasons451 :: Status
unavailableForLegalReasons451 = mkStatus 451 "Unavailable For Legal Reasons"

-- | Create a 500 Internal Server Error response with the error page.
respondInternalError :: CallStack -> Text -> PageResponse Page
respondInternalError = respondError internalServerError500

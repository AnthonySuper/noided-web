module Noided.Web.Internal.Type.Response where

import Control.Monad.Trans.Class
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Lucid.Base
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status

-- | An actual response body, which will be rendered by the server.
data ResponseBody
  = ByteStringBody ByteString
  | LazyByteStringBody LBS.ByteString
  | BuilderBody Builder
  deriving (Generic)

-- | What type of redirect to make.
data RedirectType
  = RedirectMovedPermanently
  | RedirectFound
  | RedirectSeeOther
  | RedirectTemporary
  | RedirectPermanent
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

data PageResponse renderM where
  -- | Respond with some kind of typical page.
  RespondPage :: Status -> HtmlT renderM () -> PageResponse renderM
  -- | Respond that a form had some errors.
  RespondFormErrors ::
    -- | Layout-like wrapper to be applied over the form.
    --
    -- This is only applied if the initial form request was not enhanced
    -- with fragment functionality.
    (HtmlT renderM () -> HtmlT renderM ()) ->
    -- | Actual rendered form /internals/ with errors.
    --
    -- This is used to render fragments when forms are submitted with an @Accept@
    -- header that will use a fragment.
    HtmlT renderM () ->
    PageResponse renderM
  -- | Respond with some kind of redirect.
  RespondRedirect ::
    -- | How to redirect
    RedirectType ->
    -- | Where to redirect
    Text ->
    -- | The actual redirection
    PageResponse renderM

respondPage :: Status -> HtmlT renderM () -> PageResponse renderM
respondPage = RespondPage

respondPage200 :: HtmlT renderM () -> PageResponse renderM
respondPage200 = RespondPage ok200

-- | Lift the monad in which rendering takes place.
liftPageResponseRendering ::
  (Monad m, Monad n) =>
  (forall a. m a -> n a) ->
  PageResponse m ->
  PageResponse n
liftPageResponseRendering f = \case
  RespondPage s h -> RespondPage s (hoistHtmlT f h)
  RespondFormErrors wrap inner ->
    RespondFormErrors
      ( \ht -> do
          arg' <- lift (commuteHtmlT2 ht)
          hoistHtmlT f $ wrap arg'
      )
      (hoistHtmlT f inner)
  RespondRedirect t l -> RespondRedirect t l

-- | Add a layout to the response.
addPageResponseLayout ::
  -- | Function to add a layout
  (HtmlT renderM () -> HtmlT renderM ()) ->
  PageResponse renderM ->
  PageResponse renderM
addPageResponseLayout layout = \case
  RespondPage s html -> RespondPage s (layout html)
  RespondFormErrors fLayout fInner -> RespondFormErrors (layout . fLayout) fInner
  r@(RespondRedirect {}) -> r

data PageResponseType
  = -- | Render a *full* page.
    FullPage
  | -- | Render a *fragment*.
    -- This happens if the web request has an `Accept` header compatible with noided-fragments @ application/vnd.noided-fragment @
    -- When rendering form errors, only the inner errors portion will be rendered.
    Fragment
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

pageResponseToResponse :: (Monad m) => PageResponseType -> PageResponse m -> m Response
pageResponseToResponse type_ = \case
  RespondPage s html -> do
    body <- renderBST html
    pure
      Response
        { status = s,
          headers = [("Content-Type", contentType)],
          body = LazyByteStringBody body
        }
  RespondFormErrors layout inner -> do
    let html = case type_ of
          FullPage -> layout inner
          Fragment -> inner
    body <- renderBST html
    pure
      Response
        { status = badRequest400,
          headers = [("Content-Type", contentType)],
          body = LazyByteStringBody body
        }
  RespondRedirect redirType loc ->
    let s = case redirType of
          RedirectMovedPermanently -> movedPermanently301
          RedirectFound -> found302
          RedirectSeeOther -> seeOther303
          RedirectTemporary -> temporaryRedirect307
          RedirectPermanent -> permanentRedirect308
     in pure
          Response
            { status = s,
              headers = [("Location", encodeUtf8 loc)],
              body = ByteStringBody ""
            }
  where
    contentType = case type_ of
      FullPage -> "text/html; charset=utf-8"
      Fragment -> "application/vnd.noided-fragment; charset=utf-8"

-- | A web response of some variety.
data Response
  = Response
  { -- | Actual status of the response.
    status :: Status,
    -- | Any HTTP headers added to the response.
    headers :: [(HeaderName, ByteString)],
    -- | Body of the response.
    body :: ResponseBody
  }
  deriving (Generic)

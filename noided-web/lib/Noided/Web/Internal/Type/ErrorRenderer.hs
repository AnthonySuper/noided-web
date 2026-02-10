{-# LANGUAGE OverloadedStrings #-}

module Noided.Web.Internal.Type.ErrorRenderer where

import Data.ByteString qualified as SBS
import Data.ByteString.Lazy (ByteString)
import Data.Dependent.Map qualified as DMap
import Data.Foldable (traverse_)
import Data.Maybe
import GHC.Stack
import Lucid
import Network.HTTP.Media (MediaType, mapAccept, renderHeader)
import Network.HTTP.Types.Status (internalServerError500)
import Noided.Web.Internal.Type.Response
import Noided.Web.Internal.Type.ServerError
import Type.Reflection

-- | An error renderer for a given monad and state.
newtype ErrorRenderer err
  = RenderError {runErrorRenderer :: Maybe CallStack -> err -> [(MediaType, ByteString)]}
  deriving newtype (Semigroup, Monoid)

-- | A container for multiple error renderers.
-- This is used to handle \"generic\" server errors where no meaningful user remedy is possible, such as
-- not being able to connect to a database.
newtype ErrorRenderers = MkErrorRenderers {getErrorRenderers :: DMap.DMap TypeRep (ErrorRenderer)}

instance Semigroup ErrorRenderers where
  (MkErrorRenderers a) <> (MkErrorRenderers b) = MkErrorRenderers $ DMap.unionWithKey (\_ x y -> x <> y) a b

instance Monoid ErrorRenderers where
  mempty = MkErrorRenderers mempty

-- | Lookup a generic @SomeServerError@ and render an appropriate error page, if possible.
-- If there is not an error renderer for this content type/renderer pair, we will use the fallback value.
useErrorRenderersWith ::
  -- | Default renderer to use if no media-type-specific renderer is found.
  (SomeServerError -> (MediaType, ByteString)) ->
  -- | Library of error renderers to use.
  ErrorRenderers ->
  -- | The actual server error to be rendered
  SomeServerError ->
  -- | Client-side @Accept@ header value
  SBS.ByteString ->
  -- | Renderered response.
  Response
useErrorRenderersWith fallback renderers sse@(SomeServerError _ mStack err) acceptHeader =
  toResp $ fromMaybe (fallback sse) fromRenderers
  where
    fromRenderers :: Maybe (MediaType, ByteString)
    fromRenderers = do
      let tr = typeOf err
      res <- DMap.lookup tr (getErrorRenderers renderers)
      mapAccept [(media, (media, resp)) | (media, resp) <- runErrorRenderer res mStack err] acceptHeader
    toResp :: (MediaType, ByteString) -> Response
    toResp (media, body) =
      Response
        { status = internalServerError500,
          headers = [("Content-Type", renderHeader media)],
          body = LazyByteStringBody body
        }

-- | Use error renderers, displaying a debug page if no specific renderer is found.
-- This debug page will be an HTML5 webpage including:
--
-- - The type of the error
-- - The string representation of the error
-- - A backtrace of where the error occurred
--
-- This page will have extremely basic inline styling.
--
-- As this page \"leaks\" information about the structure of your code (via the backtrace)
-- it should only be used in debug mode.
useErrorRenderersDebug :: ErrorRenderers -> SomeServerError -> SBS.ByteString -> Response
useErrorRenderersDebug = useErrorRenderersWith $ \(SomeServerError msg mStack _) ->
  ( "text/html; charset=utf-8",
    renderBS $ do
      doctype_
      html_ $ do
        head_ $ title_ "Server Error"
        body_ [style_ "font-family: sans-serif; padding: 2em;"] $ do
          h1_ "Internal Server Error"
          p_ $ do
            b_ "Error: "
            toHtml msg
          case mStack of
            Nothing -> mempty
            Just stack -> do
              h2_ "Backtrace"
              ul_ $ do
                traverse_ (li_ [] . renderEntry) (getCallStack stack)
  )
  where
    renderEntry :: (String, SrcLoc) -> Html ()
    renderEntry (fn, loc) = do
      span_ [style_ "color: #999;"] $ do
        toHtml (srcLocPackage loc)
        ":"
        toHtml (srcLocModule loc)
        " "
      b_ $ toHtml fn
      span_ [style_ "color: #999;"] $ do
        " ("
        toHtml (srcLocFile loc)
        ":"
        toHtml (show $ srcLocStartLine loc)
        ":"
        toHtml (show $ srcLocStartCol loc)
        ")"

-- | Use an error renderer that displays a generic "something went wrong" page.
-- Said page is the first argument.
useErrorRenderersAnonymous :: Html () -> ErrorRenderers -> SomeServerError -> SBS.ByteString -> Response
useErrorRenderersAnonymous page = useErrorRenderersWith $ \_ -> ("text/html; charset=utf-8", renderBS page)

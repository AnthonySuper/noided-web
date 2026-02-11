{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Type.PageAction where

import Control.Monad.Trans.Reader
import Effectful
import Effectful.Error.Static
import GHC.Generics
import Lucid.Base
import Network.HTTP.Types.Method
import Noided.Pathname
import Noided.Web.Html.Internal.Class.FetchHtmlFormatters
import Noided.Web.Html.Internal.Class.FetchMessages
import Noided.Web.Html.Internal.Type.TranslationT
import Noided.Web.Internal.Type.Response
import Optics.Core

-- | A page action, in some rendering monad, with some response.
newtype PageAction renderM actionM pathParams
  = PageAct
  { runPageAct ::
      RouteParams pathParams ->
      actionM (PageResponse renderM)
  }

aroundPageAction ::
  ( ( RouteParams pathParams1 ->
      actionM1 (PageResponse renderM1)
    ) ->
    RouteParams pathParams2 ->
    actionM2 (PageResponse renderM2)
  ) ->
  PageAction renderM1 actionM1 pathParams1 ->
  PageAction renderM2 actionM2 pathParams2
aroundPageAction f (PageAct act) = PageAct (f act)

hoistPageActionMonad ::
  (forall a. actionM1 a -> actionM2 a) ->
  PageAction renderM actionM1 pathParams ->
  PageAction renderM actionM2 pathParams
hoistPageActionMonad f (PageAct act) = PageAct (f . act)

data SomePageAction renderM actionM where
  SomePageAction ::
    StdMethod ->
    PathTemplate pathParams ->
    PageAction renderM actionM pathParams ->
    SomePageAction renderM actionM

-- | Type of declared page routes.
newtype PageRoutes renderM actionM
  = MkPageRoutes
  { routes :: [SomePageAction renderM actionM]
  }
  deriving (Generic)

instance Semigroup (PageRoutes renderM actionM) where
  a <> b =
    MkPageRoutes {routes = (a ^. #routes) <> (b ^. #routes)}

instance Monoid (PageRoutes renderM actionM) where
  mempty = MkPageRoutes mempty

-- | Add a layout to each page in the routes..
pagesAddLayout :: (Monad actionM) => (forall a. HtmlT renderM a -> HtmlT renderM a) -> PageRoutes renderM actionM -> PageRoutes renderM actionM
pagesAddLayout layout (MkPageRoutes routes) =
  MkPageRoutes $
    map
      ( \(SomePageAction method template (PageAct act)) ->
          SomePageAction method template $
            PageAct $ \params -> do
              resp <- act params
              return $ addPageResponseLayout layout resp
      )
      routes

-- | Execute some effects /around/ an action, possibly changing the response in the process.
pagesAroundAction ::
  (actionM (PageResponse renderM) -> actionM' (PageResponse renderM')) ->
  PageRoutes renderM actionM ->
  PageRoutes renderM' actionM'
pagesAroundAction f (MkPageRoutes routes) =
  MkPageRoutes $
    map
      ( \(SomePageAction method template (PageAct act)) ->
          SomePageAction method template $
            PageAct $ \params -> f (act params)
      )
      routes

-- | Do something *around* a response.
-- This is often useful to change the rendering monad, by getting context from the environment, for example.
pagesAroundResponse ::
  (Monad actionM) =>
  (PageResponse renderM -> actionM (PageResponse renderM')) ->
  PageRoutes renderM actionM ->
  PageRoutes renderM' actionM
pagesAroundResponse f (MkPageRoutes routes) =
  MkPageRoutes $
    map
      ( \(SomePageAction method template (PageAct act)) ->
          SomePageAction method template $
            PageAct $ \params -> do
              resp <- act params
              f resp
      )
      routes

-- | Provide translations from the environment to rendering pages.
pagesProvideTranslations ::
  ( FetchMessages actionM,
    FetchHtmlFormatters actionM,
    Monad actionM,
    Monad renderM'
  ) =>
  PageRoutes (TranslationT renderM') actionM ->
  PageRoutes renderM' actionM
pagesProvideTranslations = pagesAroundResponse $ \pr -> do
  env <- translationEnvFromEnv
  return $
    liftPageResponseRendering (`getTranslationT` env) pr

pagesProvideReaderEnv ::
  ( Monad actionM,
    Monad renderM'
  ) =>
  actionM a ->
  PageRoutes (ReaderT a renderM') actionM ->
  PageRoutes renderM' actionM
pagesProvideReaderEnv getEnv = pagesAroundResponse $ \pr -> do
  env <- getEnv
  return $
    liftPageResponseRendering (`runReaderT` env) pr

pagesHandleErrorM ::
  (CallStack -> e -> Eff es (PageResponse renderM')) ->
  PageRoutes renderM' (Eff (Error e : es)) ->
  PageRoutes renderM' (Eff es)
pagesHandleErrorM handleErr = pagesAroundAction $ runErrorWith handleErr

pagesHandleError ::
  (CallStack -> e -> PageResponse renderM') ->
  PageRoutes renderM' (Eff (Error e : es)) ->
  PageRoutes renderM' (Eff es)
pagesHandleError f = pagesHandleErrorM (\c -> pure . f c)

actionRoute :: (Monad actionM) => StdMethod -> PathTemplate pathParams -> (RouteParams pathParams -> actionM (PageResponse renderM)) -> PageRoutes renderM actionM
actionRoute method template act =
  mempty
    & #routes
    .~ [SomePageAction method template (PageAct act)]

-- | Route a GET action.
actGet ::
  (Monad actionM) =>
  PathTemplate pathParams ->
  (RouteParams pathParams -> actionM (PageResponse renderM)) ->
  PageRoutes renderM actionM
actGet = actionRoute GET

-- | Route a POST action.
actPost ::
  (Monad actionM) =>
  PathTemplate pathParams ->
  (RouteParams pathParams -> actionM (PageResponse renderM)) ->
  PageRoutes renderM actionM
actPost = actionRoute POST

-- | Route a HEAD action.
actHead ::
  (Monad actionM) =>
  PathTemplate pathParams ->
  (RouteParams pathParams -> actionM (PageResponse renderM)) ->
  PageRoutes renderM actionM
actHead = actionRoute HEAD

-- | Route a PUT action.
actPut ::
  (Monad actionM) =>
  PathTemplate pathParams ->
  (RouteParams pathParams -> actionM (PageResponse renderM)) ->
  PageRoutes renderM actionM
actPut = actionRoute PUT

-- | Route a DELETE action.
actDelete ::
  (Monad actionM) =>
  PathTemplate pathParams ->
  (RouteParams pathParams -> actionM (PageResponse renderM)) ->
  PageRoutes renderM actionM
actDelete = actionRoute DELETE

-- | Route a TRACE action.
actTrace ::
  (Monad actionM) =>
  PathTemplate pathParams ->
  (RouteParams pathParams -> actionM (PageResponse renderM)) ->
  PageRoutes renderM actionM
actTrace = actionRoute TRACE

-- | Route a CONNECT action.
actConnect ::
  (Monad actionM) =>
  PathTemplate pathParams ->
  (RouteParams pathParams -> actionM (PageResponse renderM)) ->
  PageRoutes renderM actionM
actConnect = actionRoute CONNECT

-- | Route an OPTIONS action.
actOptions ::
  (Monad actionM) =>
  PathTemplate pathParams ->
  (RouteParams pathParams -> actionM (PageResponse renderM)) ->
  PageRoutes renderM actionM
actOptions = actionRoute OPTIONS

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Type.PageAction where

import Data.Sequence qualified as Seq
import GHC.Generics
import Network.HTTP.Types.Method
import Noided.Pathname
import Noided.Web.Internal.Type.Response
import Noided.Web.Internal.Type.ServerError
import Optics.Core

-- | A page action, in some rendering monad, with some response.
newtype PageAction renderM actionM pathParams
  = PageAct
  { runPageAct ::
      RouteParams pathParams ->
      actionM (Either SomeServerError (PageResponse renderM))
  }

aroundPageAction ::
  ( ( RouteParams pathParams1 ->
      actionM1 (Either SomeServerError (PageResponse renderM1))
    ) ->
    RouteParams pathParams2 ->
    actionM2 (Either SomeServerError (PageResponse renderM2))
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
data PageRoutes renderM actionM
  = MkPageRoutes
  { routes :: [SomePageAction renderM actionM]
  }
  deriving (Generic)

instance Semigroup (PageRoutes renderM actionM) where
  (MkPageRoutes a) <> (MkPageRoutes b) =
    MkPageRoutes (a <> b)

instance Monoid (PageRoutes renderM actionM) where
  mempty = MkPageRoutes mempty

actionRoute :: (Monad actionM) => StdMethod -> PathTemplate pathParams -> (RouteParams pathParams -> actionM (PageResponse renderM)) -> PageRoutes renderM actionM
actionRoute method template act =
  mempty
    & #routes
    .~ [SomePageAction method template (PageAct $ fmap Right . act)]

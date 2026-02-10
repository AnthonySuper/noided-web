{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Type.PageAction where

import Data.Sequence qualified as Seq
import Network.HTTP.Types.Method
import Noided.Pathname
import Noided.Web.Internal.Type.Response
import Noided.Web.Internal.Type.ServerError

-- | A page action, in some rendering monad, with some response.
newtype PageAction actionM renderM pathParams
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
  PageAction actionM1 renderM1 pathParams1 ->
  PageAction actionM2 renderM2 pathParams2
aroundPageAction f (PageAct act) = PageAct (f act)

hoistPageActionMonad ::
  (forall a. actionM1 a -> actionM2 a) ->
  PageAction actionM1 renderM pathParams ->
  PageAction actionM2 renderM pathParams
hoistPageActionMonad f (PageAct act) = PageAct (f . act)

-- | Some page action at a known pathname.
data SomePageAction actionM renderM where
  SomePageAct ::
    StdMethod ->
    PathTemplate pathParams ->
    PageAction actionM renderM pathParams ->
    SomePageAction actionM renderM

-- | Wrapper for a list of page actions.
newtype PageActions actionM renderM
  = MkPageActions
  { getPageActions :: Seq.Seq (SomePageAction actionM renderM)
  }
  deriving newtype (Semigroup, Monoid)

hoistSomePageActionMonad ::
  (forall a. actionM1 a -> actionM2 a) ->
  SomePageAction actionM1 renderM ->
  SomePageAction actionM2 renderM
hoistSomePageActionMonad f (SomePageAct method path action) =
  SomePageAct method path (hoistPageActionMonad f action)

pageAction ::
  StdMethod ->
  PathTemplate pathParams ->
  ( RouteParams pathParams ->
    actionM (Either SomeServerError (PageResponse renderM))
  ) ->
  PageActions actionM renderM
pageAction verb pt act =
  MkPageActions
    ( pure $ SomePageAct verb pt (PageAct act)
    )

actionGet,
  actionPost,
  actionPut,
  actionPatch,
  actionDelete ::
    PathTemplate pathParams ->
    (RouteParams pathParams -> actionM (Either SomeServerError (PageResponse renderM))) ->
    PageActions actionM renderM
actionGet = pageAction GET
actionPost = pageAction POST
actionPut = pageAction PUT
actionPatch = pageAction PATCH
actionDelete = pageAction DELETE

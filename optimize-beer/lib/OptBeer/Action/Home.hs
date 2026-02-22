{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Action.Home where

import Data.Maybe (isJust)
import Effectful
import Noided.Pathname
import Noided.Sql
import Noided.Web
import OptBeer.DB.Ids.ActorId (ActorId)
import OptBeer.DB.Table.Actor
import OptBeer.DB.Table.UserDefaultOrganization
import OptBeer.Effect.CurrentActor
import OptBeer.Page.Home
import OptBeer.Page.Type (Page)
import OptBeer.Routes (homePath)

homeActions ::
  ( CurrentActor :> es,
    RunTransaction :> es
  ) =>
  PageRoutes Page (Eff es)
homeActions = actGet homePath homeAction

homeAction ::
  ( CurrentActor :> es,
    RunTransaction :> es
  ) =>
  RouteParams '[] ->
  Eff es (PageResponse Page)
homeAction (RPNil :: RouteParams '[]) = do
  mActor <- getCurrentActor
  hasDefaultOrg <- case mActor of
    Nothing -> return False
    Just actor -> do
      res <- runTransactionToResult $ queryMaybe $ do
        row <- addFrom_ (fromBase_ userDefaultOrganizationsTable)
        addWhere_ (row.userId ==. bindParam (actor.id :: ActorId))
        return row
      case res of
        TransactOK m -> return (isJust m)
        _ -> return False

  return $ respondPage200 (homePage mActor hasDefaultOrg)

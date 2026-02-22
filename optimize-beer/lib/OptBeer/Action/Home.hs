{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Action.Home where

import Noided.Pathname
import Noided.Web
import OptBeer.Page.Home
import OptBeer.Page.Type (Page)
import OptBeer.Routes (homePath)

homeActions :: (Monad m) => PageRoutes Page m
homeActions = actGet homePath homeAction

homeAction :: (Monad m) => RouteParams '[] -> m (PageResponse Page)
homeAction (RPNil :: RouteParams '[]) =
  return $ respondPage200 homePage

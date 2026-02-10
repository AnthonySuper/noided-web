{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Effect.WriteCookie where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Web.Cookie (SetCookie)
import Web.Cookie qualified as Cookie

-- | Effect for writing cookies.
data WriteCookie :: Effect where
  SetCookie :: SetCookie -> WriteCookie es ()

type instance DispatchOf WriteCookie = Dynamic

setCookie :: (WriteCookie :> es) => SetCookie -> Eff es ()
setCookie = send . SetCookie

runIgnoringCookies :: Eff (WriteCookie : es) a -> Eff es a
runIgnoringCookies = interpret $ \_ (SetCookie{}) -> return ()

runCollectingCookies :: Eff (WriteCookie : es) a -> Eff es (a, [SetCookie])
runCollectingCookies = reinterpret (runState @[SetCookie] []) $ \_ (SetCookie c) -> do
  modify @[SetCookie] (c :)
  return ()

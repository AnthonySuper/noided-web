{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Effect.GetCookies where

import Effectful
import Effectful.Dispatch.Dynamic
import Noided.Web.Internal.Type.SecureCookie

-- | Effect for reading secure cookies from a request.
data GetCookies :: Effect where
  GetCookies :: GetCookies m SecureCookies

type instance DispatchOf GetCookies = Dynamic

-- | Get cookies from the request environment.
getCookies :: (GetCookies :> es) => Eff es SecureCookies
getCookies = send GetCookies

-- | Run without any cookies.
runWithoutCookies :: Eff (GetCookies : es) a -> Eff es a
runWithoutCookies = runWithCookies mempty

-- | Run with the given static set of cookies.
runWithStaticCookies :: SecureCookies -> Eff (GetCookies : es) a -> Eff es a
runWithStaticCookies secureCookies = interpret $ \_ GetCookies -> return secureCookies

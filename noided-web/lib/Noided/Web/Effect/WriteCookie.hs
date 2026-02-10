{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Effect.WriteCookie
  ( WriteCookie,
    setCookie,
    setCookieCSRFProof,
    runIgnoringCookies,
    runCollectingCookies,
    runSettingCookies,
  )
where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Foldable
import Data.Time
import Effectful
import Noided.Web.Internal.Effect.Signing
import Noided.Web.Internal.Effect.WriteCookie
import Noided.Web.Internal.Effect.WriteHeader
import Noided.Web.Internal.Type.SecureCookie
import Web.Cookie qualified as Cookie

runSettingCookies :: (Signing :> es, WriteHeader :> es) => Eff (WriteCookie : es) b -> Eff es b
runSettingCookies act = do
  (res, cookies) <- runCollectingCookies act
  signer <- mainSigner
  traverse_ (writeHeader "Set-Cookie" . renderSecureCookieBS . toSecureCookie signer) cookies
  return res

setCookieCSRFProof ::
  ( WriteCookie :> es,
    ToJSON a
  ) =>
  Maybe UTCTime ->
  ByteString ->
  a ->
  Eff es ()
setCookieCSRFProof utcT name val =
  setCookie $
    Cookie.defaultSetCookie
      { Cookie.setCookieExpires = utcT,
        Cookie.setCookieSameSite = Just Cookie.sameSiteLax,
        Cookie.setCookieHttpOnly = True,
        Cookie.setCookieSecure = True,
        Cookie.setCookieName = name,
        Cookie.setCookieValue = toStrict (encode val)
      }

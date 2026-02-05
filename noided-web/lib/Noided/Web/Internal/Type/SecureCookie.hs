{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Internal.Type.SecureCookie
  ( SecureCookies (getSecureCookies),
    parseSecureCookies,
    SetCookieSecure (getSetCookieSecure),
    toSecureCookie,
    csrfProofCookie,
    renderSecureCookie,
    renderSecureCookieBS,
  )
where

import Data.Aeson
import Data.ByteString
import Data.ByteString.Builder (Builder)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as Enc
import Data.Time
import Noided.Web.Internal.Type.SignedMessage
import Optics.Core
import Web.Cookie qualified as Cookie

-- | All the secure cookies in a given request.
--
-- Both the cookie name and the cookie value are secured, via use of the `purpose` field.
newtype SecureCookies = UnsafeMkSecureCookies {getSecureCookies :: Map.Map Text Value}
  deriving (Show, Eq, Ord)
  deriving newtype (Semigroup, Monoid)

-- | Parse secure cookies given a list of signers.
parseSecureCookies :: (Foldable f) => f Signer -> UTCTime -> ByteString -> SecureCookies
parseSecureCookies signers utcTime cookieHeader = UnsafeMkSecureCookies . Map.fromList $ do
  (keyBS, valBS) <- Cookie.parseCookies cookieHeader
  keyText <- toListOf _Right (Enc.decodeUtf8' keyBS)
  valText <- toListOf _Right (Enc.decodeUtf8' valBS)
  signer <- toListOf folded signers
  Right val <- [verifyMessage signer utcTime (Just $ toCookiePurpose keyText) valText]
  [(keyText, getSignedValue val)]

newtype SetCookieSecure = UnsafeMkSetCookieSecure {getSetCookieSecure :: Cookie.SetCookie}

-- | Convert a normal cookie into a securely-signed cookie.
--
-- Note that the value of the cookie is assumed to be UTF-8 text.
toSecureCookie :: Signer -> Cookie.SetCookie -> SetCookieSecure
toSecureCookie signer cookie =
  UnsafeMkSetCookieSecure $
    cookie {Cookie.setCookieValue = Enc.encodeUtf8 (signSignableMessage signer cookieSignable)}
  where
    cookieSignable =
      SignableMessage
        { val = cookieVal,
          scope = Just $ toCookiePurpose cookieName,
          exp = cookieExp
        }
    cookieName = Enc.decodeUtf8Lenient (Cookie.setCookieName cookie)
    cookieVal = Enc.decodeUtf8Lenient (Cookie.setCookieValue cookie)
    cookieExp = Cookie.setCookieExpires cookie

toCookiePurpose :: Text -> Text
toCookiePurpose n = cookiePurposeBase <> "-" <> n

renderSecureCookie :: SetCookieSecure -> Builder
renderSecureCookie (UnsafeMkSetCookieSecure cookie) = Cookie.renderSetCookie cookie

renderSecureCookieBS :: SetCookieSecure -> ByteString
renderSecureCookieBS (UnsafeMkSetCookieSecure cookie) = Cookie.renderSetCookieBS cookie

-- | Make a secure cookie that is CSRF-proof, with a given expiry.
csrfProofCookie :: (ToJSON a) => Signer -> Maybe UTCTime -> ByteString -> a -> SetCookieSecure
csrfProofCookie signer utcT name val =
  toSecureCookie signer $
    Cookie.defaultSetCookie
      { Cookie.setCookieExpires = utcT,
        Cookie.setCookieSameSite = Just Cookie.sameSiteLax,
        Cookie.setCookieHttpOnly = True,
        Cookie.setCookieSecure = True,
        Cookie.setCookieName = name,
        Cookie.setCookieValue = toStrict (encode val)
      }

cookiePurposeBase :: Text
cookiePurposeBase = "cookie"

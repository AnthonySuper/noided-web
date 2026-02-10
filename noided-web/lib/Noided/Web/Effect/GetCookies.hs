{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Web.Effect.GetCookies
  ( GetCookies,
    getCookies,
    getSecureCookie,
    SecureCookies,

    -- ** Interpreters
    runWithoutCookies,
    runWithStaticCookies,
    runWithActualCookies,
  )
where

import Data.Aeson
import Data.Text
import Effectful
import Noided.Web.Internal.Effect.CurrentTime
import Noided.Web.Internal.Effect.GetCookies
import Noided.Web.Internal.Effect.Signing
import Noided.Web.Internal.Effect.SomeRequest
import Noided.Web.Internal.Type.SecureCookie
import Optics.Core

_ResultSuccess :: Prism (Result a) (Result a) a a
_ResultSuccess = prism' Success $ \case
  Success a -> Just a
  Error _ -> Nothing

getSecureCookie ::
  ( GetCookies :> es,
    FromJSON a
  ) =>
  Text ->
  Eff es (Maybe a)
getSecureCookie name = do
  cookies <- getSecureCookies <$> getCookies
  return $ cookies ^? at name % _Just % to fromJSON % _ResultSuccess

runWithActualCookies ::
  ( Signing :> es,
    CurrentTime :> es,
    GetHeaders :> es
  ) =>
  Eff (GetCookies : es) b ->
  Eff es b
runWithActualCookies act = do
  signers <- (:) <$> mainSigner <*> fallbackSigners
  ct <- getCurrentTime
  headers <- getHeaders
  let cookies = headers ^. at "Cookie" % non "" % to (parseSecureCookies signers ct)
  runWithStaticCookies cookies act

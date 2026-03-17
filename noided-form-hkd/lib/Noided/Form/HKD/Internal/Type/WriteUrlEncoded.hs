{-# LANGUAGE DerivingStrategies #-}

module Noided.Form.HKD.Internal.Type.WriteUrlEncoded where

import Control.Monad.Trans.Writer.CPS
import Data.Monoid
import Data.Text (Text)
import Noided.Form

newtype WriteUrlEncoded a
  = MkWriteUrlEncoded {unWriteUrlEncoded :: Writer (Endo [(FormCanonicalKey, Text)]) a}
  deriving newtype (Functor, Applicative, Monad)

runWriteUrlEncoded :: WriteUrlEncoded a -> (a, [(FormCanonicalKey, Text)])
runWriteUrlEncoded act =
  let (a, r) = runWriter (unWriteUrlEncoded act)
   in (a, appEndo r [])

execWriteUrlEncoded :: WriteUrlEncoded a -> [(FormCanonicalKey, Text)]
execWriteUrlEncoded = snd . runWriteUrlEncoded

tellKeyValue :: FormCanonicalKey -> Text -> WriteUrlEncoded ()
tellKeyValue k v =
  MkWriteUrlEncoded $
    tell $
      Endo ((k, v) :)

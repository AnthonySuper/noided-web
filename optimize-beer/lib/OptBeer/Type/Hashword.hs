{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Type.Hashword where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class
import Data.Password.Bcrypt qualified as BC
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import Noided.Form
import OptBeer.DB.Type.PasswordDigest
import Web.HttpApiData

newtype OpaquePassword = MkOpaquePassword {getOpaquePassword :: Text}
  deriving (Generic, Eq)
  deriving (IsString, FromHttpApiData, FromFormSubmission ct) via Text

instance Show OpaquePassword where
  show _ = "\"\""

data Hashword where
  HashwordBcrypt :: BC.PasswordHash BC.Bcrypt -> Hashword
  UnsafeHashwordNothing :: Text -> Hashword

hashwordToPasswordHash :: Hashword -> PasswordDigest
hashwordToPasswordHash =
  MkPasswordDigest . \case
    HashwordBcrypt (BC.PasswordHash h) ->
      "bcrypt-" <> h
    UnsafeHashwordNothing t ->
      "nothing-" <> t

-- | Read a password from a password hash.
hashwordFromPasswordHash :: PasswordDigest -> Maybe Hashword
hashwordFromPasswordHash (MkPasswordDigest t) =
  (HashwordBcrypt . BC.PasswordHash <$> T.stripPrefix "bcrypt-" t)
    <|> (UnsafeHashwordNothing <$> T.stripPrefix "nothing-" t)

checkHashword :: OpaquePassword -> Hashword -> BC.PasswordCheck
checkHashword (MkOpaquePassword pw) = \case
  HashwordBcrypt pc -> BC.checkPassword passwordWrapped pc
  UnsafeHashwordNothing n
    | n == pw -> BC.PasswordCheckSuccess
    | otherwise -> BC.PasswordCheckFail
  where
    passwordWrapped = BC.mkPassword pw

-- | Unsafely do not hash passwords.
--
-- This is only really useful when running tests.
unsafeDoNotHashPasswords :: (Monad m) => OpaquePassword -> m Hashword
unsafeDoNotHashPasswords = return . UnsafeHashwordNothing . getOpaquePassword

-- | Hash a password using bcrypt with some params.
hashPasswordBycryptParams :: (MonadIO f) => Int -> OpaquePassword -> f Hashword
hashPasswordBycryptParams params = fmap HashwordBcrypt . BC.hashPasswordWithParams params . BC.mkPassword . getOpaquePassword

-- | Hash a password using bcrypt, with some params.
hashPasswordBcrypt :: (MonadIO f) => OpaquePassword -> f Hashword
hashPasswordBcrypt = hashPasswordBycryptParams BC.defaultParams

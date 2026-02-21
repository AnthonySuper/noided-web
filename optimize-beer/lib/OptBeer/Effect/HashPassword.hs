{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.Effect.HashPassword where

import Effectful
import Effectful.Dispatch.Dynamic
import OptBeer.Type.Hashword

data HashPassword :: Effect where
  HashPasswordE :: OpaquePassword -> HashPassword es Hashword

type instance DispatchOf HashPassword = Dynamic

hashPassword :: (HashPassword :> es) => OpaquePassword -> Eff es Hashword
hashPassword = send . HashPasswordE

-- | Unsafely hash passwords.
--
-- Please, for the love of god, only use this in test mode.
runHashPasswordUnsafelyDoingNothing :: Eff (HashPassword : es) a -> Eff es a
runHashPasswordUnsafelyDoingNothing = interpret $ \_ -> \case
  HashPasswordE pw -> unsafeDoNotHashPasswords pw

-- | Hash passwords via bcrypt.
--
-- Uses the default bcrypt configuration.
runHashPasswordBCrypt :: (IOE :> es) => Eff (HashPassword : es) a -> Eff es a
runHashPasswordBCrypt = interpret $ \_ -> \case
  HashPasswordE pw -> hashPasswordBcrypt pw

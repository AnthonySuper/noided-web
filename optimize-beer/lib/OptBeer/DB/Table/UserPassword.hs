{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.DB.Table.UserPassword where

import GHC.Generics
import Noided.Sql.Define
import OptBeer.DB.Ids.ActorId
import OptBeer.DB.Table.Timestamps
import OptBeer.DB.Type.PasswordDigest

data UserPasswordF realm wrapper
  = UserPassword
  { userId :: Columnar (RegularColumn ActorId) realm wrapper,
    passwordDigest :: Columnar (RegularColumn PasswordDigest) realm wrapper,
    timestamps :: TimestampsF realm wrapper
  }
  deriving (Generic)

$(defineHKDTable ''UserPasswordF)

deriving instance Show UserPassword

deriving instance Eq UserPassword

deriving instance Ord UserPassword

userPasswordsTable :: HKDTableDef UserPasswordF
userPasswordsTable = hkdTableDef @UserPasswordF "user_passwords"

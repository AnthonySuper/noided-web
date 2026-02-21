{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.DB.Table.User where

import Data.Text (Text)
import GHC.Generics
import Noided.Sql.Define
import OptBeer.DB.Ids.ActorId
import OptBeer.DB.Table.Timestamps

data UserF realm wrapper
  = User
  { id :: Columnar (IdentityColumn ActorId) realm wrapper,
    email :: Columnar (RegularColumn Text) realm wrapper,
    timestamps :: TimestampsF realm wrapper
  }
  deriving (Generic)

$(defineHKDTable ''UserF)

deriving instance Show User

deriving instance Eq User

deriving instance Ord User

usersTable :: HKDTableDef UserF
usersTable = hkdTableDef @UserF "users"

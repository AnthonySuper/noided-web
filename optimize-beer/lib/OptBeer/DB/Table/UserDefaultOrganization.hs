{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.DB.Table.UserDefaultOrganization where

import GHC.Generics
import Noided.Sql.Define
import OptBeer.DB.Ids.ActorId
import OptBeer.DB.Ids.OrganizationId
import OptBeer.DB.Table.Timestamps

data UserDefaultOrganizationF realm wrapper
  = UserDefaultOrganization
  { userId :: Columnar (RegularColumn ActorId) realm wrapper,
    organizationId :: Columnar (RegularColumn OrganizationId) realm wrapper,
    timestamps :: TimestampsF realm wrapper
  }
  deriving (Generic)

$(defineHKDTable ''UserDefaultOrganizationF)

deriving instance Show UserDefaultOrganization

deriving instance Eq UserDefaultOrganization

deriving instance Ord UserDefaultOrganization

userDefaultOrganizationsTable :: HKDTableDef UserDefaultOrganizationF
userDefaultOrganizationsTable = hkdTableDef @UserDefaultOrganizationF "user_default_organizations"

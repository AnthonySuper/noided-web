{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.DB.Table.OrganizationUserAccess where

import GHC.Generics
import Noided.Sql.Define
import OptBeer.DB.Ids.ActorId
import OptBeer.DB.Ids.OrganizationId
import OptBeer.DB.Type.OrganizationAccessLevel
import OptBeer.DB.Table.Timestamps

data OrganizationUserAccessF realm wrapper
  = OrganizationUserAccess
  { organizationId :: Columnar (RegularColumn OrganizationId) realm wrapper,
    userId :: Columnar (RegularColumn ActorId) realm wrapper,
    accessLevel :: Columnar (RegularColumn OrganizationAccessLevel) realm wrapper,
    timestamps :: TimestampsF realm wrapper
  }
  deriving (Generic)

$(defineHKDTable ''OrganizationUserAccessF)

deriving instance Show OrganizationUserAccess

deriving instance Eq OrganizationUserAccess

deriving instance Ord OrganizationUserAccess

organizationUserAccessesTable :: HKDTableDef OrganizationUserAccessF
organizationUserAccessesTable = hkdTableDef @OrganizationUserAccessF "organization_user_accesses"

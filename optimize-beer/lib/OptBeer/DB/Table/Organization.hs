{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.DB.Table.Organization where

import Data.Text (Text)
import GHC.Generics
import Noided.Sql.Define
import OptBeer.DB.Ids.OrganizationId
import OptBeer.DB.Table.Timestamps

data OrganizationF realm wrapper
  = Organization
  { id :: Columnar (IdentityColumn OrganizationId) realm wrapper,
    name :: Columnar (RegularColumn Text) realm wrapper,
    timestamps :: TimestampsF realm wrapper
  }
  deriving (Generic)

$(defineHKDTable ''OrganizationF)

deriving instance Show Organization

deriving instance Eq Organization

deriving instance Ord Organization

organizationsTable :: HKDTableDef OrganizationF
organizationsTable = hkdTableDef @OrganizationF "organizations"

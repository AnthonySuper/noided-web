{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.DB.Table.Item where

import Data.Text (Text)
import GHC.Generics
import Noided.Sql.Define
import OptBeer.DB.Ids.ItemId
import OptBeer.DB.Ids.OrganizationId
import OptBeer.DB.Table.Timestamps
import OptBeer.DB.Type.Unit
import OptBeer.DB.Type.UnitCategory

data ItemF realm wrapper
  = Item
  { id :: Columnar (IdentityColumn ItemId) realm wrapper,
    organizationId :: Columnar (RegularColumn OrganizationId) realm wrapper,
    name :: Columnar (RegularColumn Text) realm wrapper,
    description :: Columnar (Column MayBeDefault NonNull Text) realm wrapper,
    defaultUnit :: Columnar (RegularColumn Unit) realm wrapper,
    measureCategory :: Columnar (Column AlwaysDefault NonNull UnitCategory) realm wrapper,
    timestamps :: TimestampsF realm wrapper
  }
  deriving (Generic)

$(defineHKDTable ''ItemF)

deriving instance Show Item

deriving instance Eq Item

deriving instance Ord Item

itemsTable :: HKDTableDef ItemF
itemsTable = hkdTableDef @ItemF "items"

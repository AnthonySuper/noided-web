{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.DB.Table.Recipe where

import Data.Int (Int32)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import Noided.Sql.Define
import OptBeer.DB.Ids.OrganizationId
import OptBeer.DB.Ids.RecipeId
import OptBeer.DB.Table.Timestamps
import OptBeer.DB.Type.Unit

data RecipeF realm wrapper
  = Recipe
  { id :: Columnar (IdentityColumn RecipeId) realm wrapper,
    organizationId :: Columnar (RegularColumn OrganizationId) realm wrapper,
    name :: Columnar (RegularColumn Text) realm wrapper,
    description :: Columnar (Column MayBeDefault NonNull Text) realm wrapper,
    
    -- Brewing Metrics
    batchSize :: Columnar (Column MayBeDefault NonNull Scientific) realm wrapper,
    batchSizeUnit :: Columnar (Column MayBeDefault NonNull Unit) realm wrapper,
    batchSizeNormalized :: Columnar (Column AlwaysDefault NonNull Scientific) realm wrapper,
    
    targetOg :: Columnar (Column MayBeDefault NonNull Scientific) realm wrapper,
    targetFg :: Columnar (Column MayBeDefault NonNull Scientific) realm wrapper,
    targetAbv :: Columnar (Column MayBeDefault NonNull Scientific) realm wrapper,
    targetIbu :: Columnar (Column MayBeDefault NonNull Int32) realm wrapper,
    targetSrm :: Columnar (Column MayBeDefault NonNull Scientific) realm wrapper,
    boilTimeMinutes :: Columnar (Column MayBeDefault NonNull Int32) realm wrapper,
    targetEfficiency :: Columnar (Column MayBeDefault NonNull Scientific) realm wrapper,

    publishedAt :: Columnar (Column NoDefault Nullable UTCTime) realm wrapper,
    deletedAt :: Columnar (Column NoDefault Nullable UTCTime) realm wrapper,
    timestamps :: TimestampsF realm wrapper
  }
  deriving (Generic)

$(defineHKDTable ''RecipeF)

deriving instance Show Recipe

deriving instance Eq Recipe

deriving instance Ord Recipe

recipesTable :: HKDTableDef RecipeF
recipesTable = hkdTableDef @RecipeF "recipes"

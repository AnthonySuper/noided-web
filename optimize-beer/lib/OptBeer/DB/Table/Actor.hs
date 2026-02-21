{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.DB.Table.Actor where

import Data.Text (Text)
import GHC.Generics
import Noided.Sql.Define
import OptBeer.DB.Ids.ActorId
import OptBeer.DB.Table.Timestamps

data ActorF realm wrapper
  = Actor
  { id :: Columnar (IdentityColumn ActorId) realm wrapper,
    name :: Columnar (RegularColumn Text) realm wrapper,
    timestamps :: TimestampsF realm wrapper
  }
  deriving (Generic)

$(defineHKDTable ''ActorF)

deriving instance Show Actor

deriving instance Eq Actor

deriving instance Ord Actor

actorsTable :: HKDTableDef ActorF
actorsTable = hkdTableDef @ActorF "actors"

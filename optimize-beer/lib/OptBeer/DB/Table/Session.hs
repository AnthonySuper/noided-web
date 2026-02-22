{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.DB.Table.Session where

import Data.IP (IPRange)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import Noided.Sql.Define
import OptBeer.DB.Ids.ActorId
import OptBeer.DB.Ids.SessionId
import OptBeer.DB.Table.Timestamps
import PostgreSQL.Binary.Range (Range)

data SessionF realm wrapper
  = Session
  { id :: Columnar (IdentityColumn SessionId) realm wrapper,
    userId :: Columnar (RegularColumn ActorId) realm wrapper,
    userAgent :: Columnar (Column NoDefault Nullable Text) realm wrapper,
    remoteIp :: Columnar (RegularColumn IPRange) realm wrapper,
    validDuring :: Columnar (RegularColumn (Range UTCTime)) realm wrapper,
    timestamps :: TimestampsF realm wrapper
  }
  deriving (Generic)

$(defineHKDTable ''SessionF)

deriving instance Show Session

deriving instance Eq Session

deriving instance Ord Session

sessionsTable :: HKDTableDef SessionF
sessionsTable = hkdTableDef @SessionF "sessions"

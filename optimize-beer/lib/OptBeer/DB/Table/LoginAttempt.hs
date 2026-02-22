{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OptBeer.DB.Table.LoginAttempt where

import Data.IP (IPRange)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import Noided.Sql.Define
import OptBeer.DB.Ids.ActorId
import OptBeer.DB.Ids.LoginAttemptId

data LoginAttemptF realm wrapper
  = LoginAttempt
  { id :: Columnar (IdentityColumn LoginAttemptId) realm wrapper,
    userId :: Columnar (RegularColumn ActorId) realm wrapper,
    userAgent :: Columnar (Column NoDefault Nullable Text) realm wrapper,
    remoteIp :: Columnar (RegularColumn IPRange) realm wrapper,
    attemptAt :: Columnar (RegularColumn UTCTime) realm wrapper,
    successful :: Columnar (RegularColumn Bool) realm wrapper
  }
  deriving (Generic)

$(defineHKDTable ''LoginAttemptF)

deriving instance Show LoginAttempt

deriving instance Eq LoginAttempt

deriving instance Ord LoginAttempt

loginAttemptsTable :: HKDTableDef LoginAttemptF
loginAttemptsTable = hkdTableDef @LoginAttemptF "login_attempts"

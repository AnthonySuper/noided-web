{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.DB.Table.Timestamps where

import Data.Time (UTCTime)
import GHC.Generics
import Noided.Row
import Noided.Sql.Define

data TimestampsF realm wrapper
  = Timestamps
  { createdAt :: Columnar (Column MayBeDefault NonNull UTCTime) realm wrapper,
    updatedAt :: Columnar (Column MayBeDefault NonNull UTCTime) realm wrapper
  }
  deriving (Generic)

$(defineHKDTable ''TimestampsF)

deriving instance Show Timestamps

deriving instance Eq Timestamps

deriving instance Ord Timestamps

deriving instance Read Timestamps

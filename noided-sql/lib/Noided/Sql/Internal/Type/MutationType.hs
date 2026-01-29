module Noided.Sql.Internal.Type.MutationType where

import GHC.Generics
import Noided.Sql.Internal.Type.SqlType

-- | Data kind, used for mutation values.
-- This is either an SQL value, or DEFAULT
data MutationType
  = -- | Insert or update an actual value.
    ActualValue SqlType
  | -- | Insert or update a @ DEFAULT @ value.
    DefaultValue
  deriving (Generic)

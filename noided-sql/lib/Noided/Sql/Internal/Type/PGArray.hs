module Noided.Sql.Internal.Type.PGArray where

import Noided.Sql.Internal.Type.SqlType

-- | Wrapper type for Postgres arrays.
data PGArray (t :: SqlType)

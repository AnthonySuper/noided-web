module Noided.Sql.Insert
  ( InsertQuery,
    insertReturning,
    insertReturningAll,
    insertDefaultValuesReturning,

    -- * Insert values
    InsertValues (..),
    InsertForTable,
  )
where

import Noided.Sql.Internal.Insert.Insert
import Noided.Sql.Internal.Insert.InsertValues

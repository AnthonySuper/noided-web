module Noided.Sql.Insert
  ( InsertQuery,
    insertReturning,
    insertReturningAll,
    insertDefaultValuesReturning,

    -- * Insert values
    InsertValues,
    defaultValues_,
    values_,
    insertSelect_,
    InsertForTable,
  )
where

import Noided.Sql.Internal.Insert.Insert
import Noided.Sql.Internal.Insert.InsertValues

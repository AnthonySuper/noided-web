module Noided.Sql.Insert
  ( InsertQuery,
    insertReturning_,
    insertReturningAll_,
    insertDefaultValuesReturning_,

    -- * Insert values
    InsertValues,
    defaultValues_,
    values_,
    singleValue_,
    insertSelect_,
    InsertForTable,
  )
where

import Noided.Sql.Internal.Insert.Insert
import Noided.Sql.Internal.Insert.InsertValues

module Noided.Sql.Update
  ( FromM,
    WhereM,
    addFromItem_,
    addWhereCondition_,
    UpdateQuery,
    updateReturning,
    update,
    updateReturningAll,

    -- * Column updates
    ColumnUpdates,
    UpdatedColumn,
    updateSet_,
    (|=),
  )
where

import Noided.Sql.Internal.Select.SelectM (FromM, WhereM, addFromItem_, addWhereCondition_)
import Noided.Sql.Internal.Update.Sets
import Noided.Sql.Internal.Update.Update

module Noided.Sql.Update
  ( OptionalFrom,
    noFrom_,
    from_,
    WhereM,
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

import Noided.Sql.Internal.Select.SelectM (WhereM, addWhereCondition_)
import Noided.Sql.Internal.Select.FromClause (OptionalFrom, noFrom_, from_)
import Noided.Sql.Internal.Update.Sets
import Noided.Sql.Internal.Update.Update

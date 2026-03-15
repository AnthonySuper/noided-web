module Noided.Sql.Update
  ( UpdateQuery,
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

import Noided.Sql.Internal.Update.Sets
import Noided.Sql.Internal.Update.Update

module Noided.Sql.Delete
  ( FromM,
    WhereM,
    addFromItem_,
    addWhereCondition_,
    DeleteQuery,
    deleteReturning,
    delete,
  )
where

import Noided.Sql.Internal.Select.SelectM (FromM, WhereM, addFromItem_, addWhereCondition_)
import Noided.Sql.Internal.Delete.Delete

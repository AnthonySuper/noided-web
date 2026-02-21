module Noided.Sql.Internal.Type.ViewColumnar where

import Data.Kind
import Noided.Sql.Internal.Class.AsHaskellValue
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlType

-- | Where a view column is being used.
-- Similar to 'ColumnUsage', but without the 'InTableDef' case since views
-- cannot be used to perform insert, update, delete, or merge operations.
data ViewColumnUsage (argument :: Type) where
  -- | In a query.
  -- In this case, we need an argument of 'SqlType -> Type'.
  ViewInQuery :: ViewColumnUsage (SqlType -> Type)
  -- | Column nullified in a query (e.g., from a LEFT JOIN).
  ViewNullifiedInQuery :: ViewColumnUsage (SqlType -> Type)
  -- | In Haskell.
  ViewInHaskell :: ViewColumnUsage ()
  -- | Nullified in Haskell.
  -- All fields will be wrapped in a 'Maybe' type.
  ViewNullifiedInHaskell :: ViewColumnUsage ()

type ViewNullifiedSqlType :: SqlType -> SqlType
type family ViewNullifiedSqlType st where
  ViewNullifiedSqlType (SqlT _ t) = SqlT Nullable t

-- | Field wrapper for view columnar values.
-- Similar to 'Columnar', but without the 'InTableDef' case since view types
-- are select-only.
--
-- Note that a type defined with 'ViewColumnar' does not /have/ to actually
-- map onto a particular SQL view — it is perfectly fine to populate the fields
-- yourself as the return value of a 'SelectM', for example.
--
-- The type parameter @st@ is a 'SqlType' (e.g. @'NonNullT' 'Int64'@), rather
-- than a 'ColumnType', since there is no concept of column defaults for views.
type ViewColumnar :: SqlType -> ViewColumnUsage argument -> argument -> Type
type family ViewColumnar st r arg where
  ViewColumnar st ViewInQuery arg = arg st
  ViewColumnar st ViewNullifiedInQuery arg = arg (ViewNullifiedSqlType st)
  ViewColumnar st ViewInHaskell '() = HaskellValueType st
  ViewColumnar (SqlT _ t) ViewNullifiedInHaskell '() = Maybe (HaskellTypeOf t)

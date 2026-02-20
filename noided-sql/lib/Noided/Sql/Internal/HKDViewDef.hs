{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Noided.Sql.Internal.HKDViewDef where

import Data.Kind
import GHC.Generics
import Noided.Sql.Internal.HKDTableDef (GSnakeCasedNames, genericSnakeCasedNames)
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.ViewColumnar
import Noided.Sql.Internal.Type.ViewDefinition
import Noided.Sql.Internal.Type.TableName

-- | Define a view using an HKD structure.
-- This automatically generates snake_cased column names from the record field names.
-- It also supports nested HKD structures, where nested field names are also snake_cased.
--
-- Note that the resulting 'ViewDef' does not /have/ to correspond to an actual
-- SQL view in the database — you can also use it as the result type of an
-- arbitrary 'SelectM'.
--
-- Example:
--
-- > data UserViewF realm f = UserView
-- >   { userId   :: ViewColumnar (NonNullT Int64) realm f,
-- >     userName :: ViewColumnar (NonNullT Text)  realm f
-- >   }
-- >   deriving (Generic)
-- >
-- > $(defineHKDView ''UserViewF)
-- >
-- > userView :: ViewDef UserViewInQuery
-- > userView = hkdViewDef "user_view"
--
-- This will result in a view definition where the columns are named
-- @\"user_id\"@ and @\"user_name\"@.
hkdViewDef ::
  forall (hkdView :: forall arg. ViewColumnUsage arg -> arg -> Type).
  ( Generic (hkdView ViewInQuery ColumnName),
    GSnakeCasedNames (Rep (hkdView ViewInQuery ColumnName))
  ) =>
  TableName ->
  ViewDef (hkdView ViewInQuery)
hkdViewDef vn =
  DefineView
    { viewName = vn,
      viewSelectedNames = to (genericSnakeCasedNames @(Rep (hkdView ViewInQuery ColumnName)))
    }

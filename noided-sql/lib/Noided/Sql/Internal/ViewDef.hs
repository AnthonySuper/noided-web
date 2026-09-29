{-# LANGUAGE AllowAmbiguousTypes #-}

module Noided.Sql.Internal.ViewDef
  ( viewSnakeCased,
    viewWithNames,
  )
where

import GHC.Generics
import Noided.Sql.Internal.SnakeCase (GSnakeCasedNames, genericSnakeCasedNames)
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.TableName (TableName)
import Noided.Sql.Internal.Type.ViewDefinition

-- | Build a 'ViewDef' for a type defined with
-- 'Noided.Sql.Internal.TH.Table.deriveView' (or 'Noided.Sql.Internal.TH.Table.deriveTable'),
-- naming every column after its field, snake_cased.
--
-- Note that the resulting 'ViewDef' does not /have/ to correspond to an actual
-- SQL view in the database; you can also use it as the result type of an
-- arbitrary 'SelectM'.
--
-- > userSummaryView :: ViewDef UserSummaryF
-- > userSummaryView = viewSnakeCased "user_summary"
viewSnakeCased ::
  forall t.
  ( Generic (t ColumnName),
    GSnakeCasedNames (Rep (t ColumnName))
  ) =>
  TableName ->
  ViewDef t
viewSnakeCased vn =
  viewWithNames vn (to (genericSnakeCasedNames @(Rep (t ColumnName))))

-- | Build a 'ViewDef' with explicit column names.
viewWithNames :: TableName -> t ColumnName -> ViewDef t
viewWithNames vn names =
  DefineView
    { viewName = vn,
      viewSelectedNames = names
    }

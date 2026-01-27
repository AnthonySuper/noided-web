{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Noided.Sql.Internal.Update.Sets where

import Data.Coerce
import Data.Kind
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits hiding (Text)
import GHC.TypeLits qualified as TL (ErrorMessage (Text))
import Noided.Row
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.ColumnType
import Noided.Sql.Internal.Type.MutationExpr (MutationExpr, unsafeMutationExprToSyntax)
import Noided.Sql.Internal.Type.MutationType
import Noided.Sql.Internal.Type.QueryWriter (QueryWriter, writeSyntax)
import Noided.Sql.Internal.Type.SqlExpr (CastNullability)
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax

-- | A reference to which column we should update.
-- You should generally use @ -XOverloadedLabels @ to generate this.
type UpdatedColumn :: [RowLabel ColumnType] -> ColumnType -> Type
data UpdatedColumn tableLabels updatedType where
  UpdateCol ::
    (forall wrapper. WrappedRow tableLabels wrapper -> wrapper updatedType) ->
    UpdatedColumn tableLabels updatedType

instance
  (RowHasLabelWithType symbol tableLabels updatedType) =>
  IsLabel symbol (UpdatedColumn tableLabels updatedType)
  where
  fromLabel = UpdateCol (getField @symbol)

-- | Build an update value from a column type and a mutation type.
type AsUpdateValue :: ColumnType -> MutationType -> Constraint
class AsUpdateValue columnType mutationType where
  toUpdateValue :: ColumnName columnType -> ColumnName mutationType

instance
  ( TypeError
      (TL.Text "Cannot use a default value as column has been marked " :<>: ShowType NoDefault)
  ) =>
  AsUpdateValue (Column NoDefault a b) DefaultValue
  where
  toUpdateValue = error "impossible"

instance AsUpdateValue (Column MayBeDefault a b) DefaultValue where
  toUpdateValue = coerce

instance (n ~ DefaultValue) => AsUpdateValue (Column AlwaysDefault a b) n where
  toUpdateValue = coerce

instance
  {-# OVERLAPS #-}
  (CastNullability definedNull insertedNull, a ~ b) =>
  AsUpdateValue (Column na insertedNull a) (ActualValue (SqlT definedNull b))
  where
  toUpdateValue = coerce

data ColumnUpdate tableLabels where
  ColUpdate ::
    (AsUpdateValue columnType mutationType) =>
    UpdatedColumn tableLabels columnType ->
    MutationExpr mutationType ->
    ColumnUpdate tableLabels

-- | A list of column set expressions for a given table.
--
-- Note that if you try to set a column with the same name twice, the *last* one wins.
newtype ColumnUpdates labels = ColUpdates {getColUpdates :: NonEmpty (ColumnUpdate labels)}
  deriving newtype (Semigroup)

updateSet_, (|=) :: (AsUpdateValue columnType mutationType) => UpdatedColumn tableLabels columnType -> MutationExpr mutationType -> ColumnUpdates tableLabels
updateSet_ col val = ColUpdates $ NE.singleton $ ColUpdate col val

infix 4 |=

(|=) = updateSet_

-- | Writes all the items in the SET clause of an update.
writeUpdateSets :: forall labels. ColumnUpdates labels -> WrappedRow labels ColumnName -> QueryWriter ()
writeUpdateSets (ColUpdates updates) row = writeSyntax finalSyntax
  where
    toPair :: ColumnUpdate labels -> (Text, Syntax)
    toPair (ColUpdate (UpdateCol accessor) expr) =
      let (MkColumnName name) = accessor row
          val = unsafeMutationExprToSyntax expr
       in (name, val)

    -- Map.fromList keeps the last occurrence for a key, satisfying "last one wins".
    updateMap :: Map.Map Text Syntax
    updateMap = Map.fromList $ NE.toList $ fmap toPair updates

    renderUpdate :: Text -> Syntax -> Syntax
    renderUpdate name val = syntaxFromText ("\"" <> name <> "\"") <> " = " <> val

    finalSyntax = fromCommaSepSyntax $ foldMap (\(k, v) -> Written (renderUpdate k v)) (Map.toList updateMap)

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Noided.Sql.Internal.Insert.InsertValues
  ( InsertValues (..),
    writeInsertValues,
    InsertForTable,
    writeColumnListForInsert,
  )
where

import Data.Coerce
import Data.HKD
import Data.Kind
import Data.List.NonEmpty qualified as NE
import GHC.Records
import GHC.TypeLits
import Noided.Row
import Noided.Sql.Internal.Class.Query (SelectQuery, Query(..))
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.ColumnType
import Noided.Sql.Internal.Type.MutationExpr
import Noided.Sql.Internal.Type.MutationType
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax

type MapSqlTypeToMutationType :: [RowLabel SqlType] -> [RowLabel MutationType]
type family MapSqlTypeToMutationType labels where
  MapSqlTypeToMutationType '[] = '[]
  MapSqlTypeToMutationType (l :=> t ': rest) =
    l :=> ActualValue t ': MapSqlTypeToMutationType rest

-- | Insert values, with given labels and names.
data InsertValues (insertedLabels :: [RowLabel MutationType]) where
  DefaultValues :: InsertValues '[]
  ValuesList ::
    (labels ~ x ': xs) =>
    NE.NonEmpty (WrappedRow labels MutationExpr) ->
    InsertValues labels
  InsertSelect ::
    ( SelectQuery query,
      QuerySelectList query ~ WrappedRow selectLabels,
      labels ~ MapSqlTypeToMutationType selectLabels
    ) =>
    query ->
    InsertValues labels

writeInsertValues :: InsertValues values -> QueryWriter ()
writeInsertValues = \case
  DefaultValues -> "DEFAULT VALUES"
  ValuesList vl -> do
    "VALUES "
    let runMap x = Written $ "(" <> fromCommaSepSyntax (ffoldMap (Written . unsafeMutationExprToSyntax) x) <> ")"
    writeSyntax $
      fromCommaSepSyntax $
        foldMap runMap vl
    pure ()
  InsertSelect q -> writeQuerySyntax q

writeColumnListForInsert ::
  forall colDefs insertedLabels.
  (AsInsertList colDefs insertedLabels) =>
  WrappedRow colDefs ColumnName ->
  InsertValues insertedLabels ->
  QueryWriter ()
writeColumnListForInsert colDefs = \case
  DefaultValues -> pure ()
  ValuesList _ -> do
    let cols :: WrappedRow insertedLabels ColumnName
        cols = insertedColumnsList colDefs
    let colSyntax = fromCommaSepSyntax $ ffoldMap (\(MkColumnName n) -> Written $ "\"" <> syntaxFromText n <> "\"") cols
    " ("
    writeSyntax colSyntax
    ")"
  InsertSelect _ -> do
    let cols :: WrappedRow insertedLabels ColumnName
        cols = insertedColumnsList colDefs
    let colSyntax = fromCommaSepSyntax $ ffoldMap (\(MkColumnName n) -> Written $ "\"" <> syntaxFromText n <> "\"") cols
    " ("
    writeSyntax colSyntax
    ")"

type AsInsertValue :: ColumnType -> MutationType -> Constraint
class AsInsertValue colType mutationType where
  toInsertValue :: ColumnName colType -> ColumnName mutationType

instance
  ( TypeError
      (Text "Cannot use a default value as column has been marked " :<>: ShowType NoDefault)
  ) =>
  AsInsertValue (Column NoDefault a b) DefaultValue
  where
  toInsertValue = error "impossible"

instance AsInsertValue (Column MayBeDefault a b) DefaultValue where
  toInsertValue = coerce

instance (n ~ DefaultValue) => AsInsertValue (Column AlwaysDefault a b) n where
  toInsertValue = coerce

instance
  {-# OVERLAPS #-}
  (CastNullability definedNull insertedNull, a ~ b) =>
  AsInsertValue (Column na insertedNull a) (ActualValue (SqlT definedNull b))
  where
  toInsertValue = coerce

type AsInsertList :: [RowLabel ColumnType] -> [RowLabel MutationType] -> Constraint
class AsInsertList colDefs insertedLabels where
  insertedColumnsList ::
    WrappedRow colDefs ColumnName ->
    WrappedRow insertedLabels ColumnName

instance {-# OVERLAPPABLE #-} AsInsertList colDefs '[] where
  insertedColumnsList _ = EmptyWrappedRow

instance
  ( AsInsertList colDefs rest,
    RowHasLabelWithType label colDefs defOfColumn,
    AsInsertValue defOfColumn val
  ) =>
  AsInsertList colDefs (label :=> val ': rest)
  where
  insertedColumnsList defs =
    toInsertValue (getField @label defs)
      :::% insertedColumnsList defs

type InsertValuePresentRec :: Symbol -> [RowLabel MutationType] -> [RowLabel MutationType] -> Constraint
type family InsertValuePresentRec label mutations allMutations where
  InsertValuePresentRec label '[] allMutations =
    TypeError
      ( Text "Required Column "
          :<>: ShowType label
          :<>: Text " is not present in insert list "
          :<>: ShowType allMutations
      )
  InsertValuePresentRec label (label :=> _ ': _) _ = ()
  InsertValuePresentRec label (_ :=> _ ': rest) am =
    InsertValuePresentRec label rest am

type RequiredInsertValuesPresent :: [RowLabel ColumnType] -> [RowLabel MutationType] -> Constraint
type family RequiredInsertValuesPresent definedColumns insertedColumns where
  RequiredInsertValuesPresent (label :=> Column NoDefault NonNull a ': xs) inserted =
    ( InsertValuePresentRec label inserted inserted,
      RequiredInsertValuesPresent xs inserted
    )
  RequiredInsertValuesPresent (_ :=> _ ': xs) inserted =
    RequiredInsertValuesPresent xs inserted
  RequiredInsertValuesPresent '[] _ = ()

type AllColumnsDefaulted :: [RowLabel ColumnType] -> Constraint
type family AllColumnsDefaulted cols where
  AllColumnsDefaulted '[] = ()
  AllColumnsDefaulted (label :=> Column NoDefault NonNull a ': _) =
    TypeError (Text "Column " :<>: ShowType label :<>: Text " has no default value, so you must insert something besides DEFAULT VALUES")
  AllColumnsDefaulted (_ :=> _ ': rest) =
    AllColumnsDefaulted rest

class (AsInsertList colDefs insertedLabels) => InsertForTable colDefs insertedLabels

instance
  {-# OVERLAPPABLE #-}
  ( AsInsertList colDefs insertedLabels,
    RequiredInsertValuesPresent colDefs insertedLabels
  ) =>
  InsertForTable colDefs insertedLabels

instance {-# OVERLAPS #-} (AsInsertList '[] others, TypeError (Text "Cannot insert into a table with no columns")) => InsertForTable '[] others

instance {-# OVERLAPS #-} (AllColumnsDefaulted colDefs) => InsertForTable colDefs '[]

instance {-# OVERLAPS #-} (TypeError (Text "Cannot insert into a table with no columns")) => InsertForTable '[] '[]

module Noided.Row
  ( RowLabel ((:=>)),
    RowElement ((:==>)),

    -- * Rows with unwrapped elements
    Row (MkRow, (:::), (:::?), EmptyRow),
    rowAppend,
    rowApplyWrapper,
    RowLabelsApplyWrapper,
    RowHasLabelWithType,

    -- * Rows with wrapped elements
    WrappedRow (EmptyWrappedRow, (:::%), (:::%?), WrappedRowCons),
    RowAppendLabels,
    wrappedRowAppend,
    wrappedRowElementL,
    wrappedRowGetFirstTyped,
    rowDivided,
    WrappedElementLensOf (..),
    wrappedRowElementLenses,
    RowKnownFields,
    rowKnownFields,
    RowFieldOf,
    RowHasFieldLabeled,
    rowFieldLabeled,
    rowFieldTypeRep,
    rowFieldLens,
    SomeField (..),
    withSomeField,
    withSomeFieldLens,
    someFieldNamed,
    someFieldFieldName,

    -- * Turn a functor of rows into a row of functors
    UnzipRow (..),

    -- ** Wrapper for class-membership evidence
    ConstraintRow (..),
    ConstraintOf (..),

    -- ** With Labels
    RowKnownLabels (..),
    wrappedRowKnownLabelStrings,
    Labeled (..),
    wrappedRowLabeled,

    -- ** HKD to and from wrapped rows:
    HKDToRow (..),
    RowToHKD (..),

    -- ** Utility: wrap/unwrap HKDs
    HKDWrap (..),
    HKDUnwrap (..),
    (:**:) (..),
    hkdWrapped,
    hkdUnwrapped,
    --- *** Via Generics
    GHKDWrap,
    gHkdWrap,
    GHKDUnwrap,
    gHkdUnwrap,
    gHkdWrapped,
    gHkdUnwrapped,

    -- ** Selecting some field
    hasFieldLabel,

    -- ** Subrows
    Subrow (..),
    subrow,
    RecordSubrow,
    recordSubrow,
    RecordSubrowWrapped (..),
  )
where

import Noided.Row.Internal.ConstraintRow
import Noided.Row.Internal.Divide
import Noided.Row.Internal.HKDToRow
import Noided.Row.Internal.HKDWrapping
import Noided.Row.Internal.KnownLabels
import Noided.Row.Internal.RowToHKD
import Noided.Row.Internal.SomeField
import Noided.Row.Internal.Subrow
import Noided.Row.Internal.Type.Row
import Noided.Row.Internal.Type.RowElement
import Noided.Row.Internal.Type.RowLabel
import Noided.Row.Internal.Type.WrappedRow
import Noided.Row.Internal.Unzip

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Row.Internal.Type.RowLabel
  ( RowLabel (..),
    RowLabelsOf,
    RowLabelsLength,
    RowAppendLabels,
    RowLabelsIndexByLabel,
    RowLabelsIndexByType,
    RowLabelsReplaceLabelByType,
    RowIndexByLabelEvidence (..),
    RowIndexByLabelEvidenceIndex,
    RowIndexByLabelEvidenceType,
    RowLabelsApplyWrapper,
    RowIndexOfOrdinalEvidence (..),
    RowIndexOfOrdinalEvidenceLabel,
    RowIndexOfOrdinalEvidenceWrapped,
    RowLabelsIndexByOrdinal,
  )
where

import GHC.TypeLits

-- | Data kind, used to label a row.
data RowLabel k where
  (:=>) :: Symbol -> k -> RowLabel k

type RowLabelsOf k = [RowLabel k]

type RowLabelsLengthRec :: [RowLabel k] -> Nat -> Nat
type family RowLabelsLengthRec lbls acc where
  RowLabelsLengthRec '[] acc = acc
  RowLabelsLengthRec (_ ': x) a =
    RowLabelsLengthRec x (a + 1)

type RowLabelsLength :: [RowLabel k] -> Nat
type RowLabelsLength lbls = RowLabelsLengthRec lbls 0

type RowAppendLabels :: [RowLabel k] -> [RowLabel k] -> [RowLabel k]
type family RowAppendLabels lhs rhs where
  RowAppendLabels '[] rhs = rhs
  RowAppendLabels (x ': r) rhs = x ': RowAppendLabels r rhs

data RowIndexByLabelEvidence k = RowIndexOfLabel Nat k

type RowIndexByLabelEvidenceIndex :: RowIndexByLabelEvidence k -> Nat
type family RowIndexByLabelEvidenceIndex l where
  RowIndexByLabelEvidenceIndex (RowIndexOfLabel n _) = n

type RowIndexByLabelEvidenceType :: RowIndexByLabelEvidence k -> k
type family RowIndexByLabelEvidenceType l where
  RowIndexByLabelEvidenceType (RowIndexOfLabel _ t) = t

type RowLabelsIndexByLabelRec ::
  Symbol ->
  [RowLabel k] ->
  [RowLabel k] ->
  Nat ->
  RowIndexByLabelEvidence k
type family RowLabelsIndexByLabelRec lbl labels allLabels recI where
  RowLabelsIndexByLabelRec label '[] allLabels _ =
    TypeError
      ( Text "Label "
          :<>: ShowType label
          :<>: Text " is not present in row with labels "
          :<>: ShowType allLabels
      )
  RowLabelsIndexByLabelRec label (label :=> t ': _) _ recI = RowIndexOfLabel recI t
  RowLabelsIndexByLabelRec label (label' :=> _ ': r) allLabels recI =
    RowLabelsIndexByLabelRec label r allLabels (recI + 1)

type RowLabelsIndexByLabel :: Symbol -> [RowLabel k] -> RowIndexByLabelEvidence k
type RowLabelsIndexByLabel label labels =
  RowLabelsIndexByLabelRec label labels labels 0

type RowLabelsReplaceLabelByTypeRec ::
  Symbol ->
  k ->
  [RowLabel k] ->
  [RowLabel k] ->
  [RowLabel k]
type family RowLabelsReplaceLabelByTypeRec sym k labels allLabels where
  RowLabelsReplaceLabelByTypeRec sym _ '[] allLabels =
    TypeError
      ( Text "Label "
          :<>: ShowType sym
          :<>: Text " is not present in row with labels "
          :<>: ShowType allLabels
      )
  RowLabelsReplaceLabelByTypeRec label t (label :=> _ ': rest) _ =
    label :=> t ': rest
  RowLabelsReplaceLabelByTypeRec label t (label' :=> t' ': rest) allLabels =
    label' :=> t' ': RowLabelsReplaceLabelByTypeRec label t rest allLabels

type RowLabelsReplaceLabelByType :: Symbol -> k -> [RowLabel k] -> [RowLabel k]
type RowLabelsReplaceLabelByType label t labels =
  RowLabelsReplaceLabelByTypeRec label t labels labels

type RowLabelsIndexByTypeRec ::
  k ->
  [RowLabel k] ->
  [RowLabel k] ->
  Nat ->
  Nat
type family RowLabelsIndexByTypeRec t labels allLabels recI where
  RowLabelsIndexByTypeRec t '[] allLabels _ =
    TypeError
      ( Text "Element with type "
          :<>: ShowType t
          :<>: Text " is not present in row with labels "
          :<>: ShowType allLabels
      )
  RowLabelsIndexByTypeRec t (_ :=> t ': _) _ recI = recI
  RowLabelsIndexByTypeRec t (_ :=> t' ': r) allLabels recI =
    RowLabelsIndexByTypeRec t r allLabels (recI + 1)

type RowLabelsIndexByType :: k -> [RowLabel k] -> Nat
type RowLabelsIndexByType t labels =
  RowLabelsIndexByTypeRec t labels labels 0

type RowLabelsApplyWrapper :: (k -> k2) -> [RowLabel k] -> [RowLabel k2]
type family RowLabelsApplyWrapper f rl where
  RowLabelsApplyWrapper _ '[] = '[]
  RowLabelsApplyWrapper f (l :=> t ': r) =
    l :=> f t ': RowLabelsApplyWrapper f r

data RowIndexOfOrdinalEvidence k = RowIndexOrdinalE Symbol k

type family RowIndexOfOrdinalEvidenceLabel k where
  RowIndexOfOrdinalEvidenceLabel (RowIndexOrdinalE lbl _) = lbl

type family RowIndexOfOrdinalEvidenceWrapped k where
  RowIndexOfOrdinalEvidenceWrapped (RowIndexOrdinalE _ wrapped) = wrapped

type RowLabelsIndexByOrdinalRec ::
  Nat -> Nat -> [RowLabel k] -> [RowLabel k] -> RowIndexOfOrdinalEvidence k
type family RowLabelsIndexByOrdinalRec c n rl allL where
  RowLabelsIndexByOrdinalRec leftover counted '[] allL =
    TypeError
      ( Text "Row did not have an element at ordinal "
          :<>: ShowType (leftover + counted)
          :<>: Text " as row labels "
          :<>: ShowType allL
          :<>: Text " were only "
          :<>: ShowType counted
          :<>: Text " elements long"
      )
  RowLabelsIndexByOrdinalRec 0 c (name :=> val ': _) _ =
    RowIndexOrdinalE name val
  RowLabelsIndexByOrdinalRec c oc (_ ': rest) allL =
    RowLabelsIndexByOrdinalRec (c - 1) (oc + 1) rest allL

type RowLabelsIndexByOrdinal n labels = RowLabelsIndexByOrdinalRec n 0 labels labels

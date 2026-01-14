module Noided.Form.Internal.Parse (fromKeysAndValues) where

import Data.Foldable
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Noided.Form.Internal.Type.FormInputKey
import Noided.Form.Internal.Type.FormSubmission
import Optics.Core

-- | Build a new form submission from input keys and input values.
fromKeysAndValues :: (Foldable f) => f (FormInputKey, Maybe (FormValue ct)) -> FormSubmission ct
fromKeysAndValues = foldl' f SubmissionEmpty
  where
    f fc (fk, fv) = insertNewPiece fc fk fv

asValue :: Maybe (FormValue contentType) -> FormSubmission contentType
asValue = maybe SubmissionEmpty SubmissionValue

insertNewPiece :: FormSubmission ct -> FormInputKey -> Maybe (FormValue ct) -> FormSubmission ct
insertNewPiece submission ik val =
  case (submission, ik) of
    (_, Seq.Empty) -> asValue val
    (SubmissionObject obj, TextPiece t Seq.:<| r) ->
      SubmissionObject $
        (at t % non' _SubmissionEmpty %~ (\x -> insertNewPiece x r val)) obj
    (_, TextPiece t Seq.:<| r) ->
      SubmissionObject $ Map.singleton t (insertNewPiece SubmissionEmpty r val)
    (SubmissionArray arr@(initSeq Seq.:|> obj@(SubmissionObject o)), BracesPiece Seq.:<| restKey@(TextPiece nestedKey Seq.:<| afterNested))
      | Just (SubmissionArray _) <- Map.lookup nestedKey o
      , BracesPiece Seq.:<| _ <- afterNested ->
          SubmissionArray $ initSeq Seq.:|> insertNewPiece obj restKey val
      | Map.member nestedKey o -> SubmissionArray $ arr Seq.:|> insertNewPiece SubmissionEmpty restKey val
      | otherwise -> SubmissionArray $ initSeq Seq.:|> insertNewPiece obj restKey val
    (SubmissionArray arr, BracesPiece Seq.:<| rest) ->
      SubmissionArray $ arr Seq.:|> insertNewPiece SubmissionEmpty rest val
    (_, BracesPiece Seq.:<| rest) ->
      SubmissionArray $ pure $ insertNewPiece SubmissionEmpty rest val

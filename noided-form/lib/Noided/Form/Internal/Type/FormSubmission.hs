{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Form.Internal.Type.FormSubmission where

import Control.DeepSeq
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Text (Text)
import GHC.Generics
import Noided.Form.Internal.Type.FormCanonicalKey
import Noided.Form.Internal.Type.FormContentType
import Noided.Form.Internal.Type.UploadedFile
import Optics.Core

-- | A single value that has been submitted from a form.
type FormValue :: FormContentType -> Type
data FormValue contentType where
  -- | A text value. Can be of either content type.
  TextValue :: !Text -> FormValue anyType
  -- | An uploaded file value. Can only be used with multipart submissions.
  FileValue :: !UploadedFile -> FormValue MultipartFormData

instance Show (FormValue ct) where
  showsPrec p = \case
    TextValue t ->
      showParen (p > 10) $
        showString "TextValue "
          . showsPrec 11 t
    FileValue f ->
      showParen (p > 10) $
        showString "FileValue "
          . showsPrec 11 f

instance Eq (FormValue ct) where
  (TextValue v) == (TextValue v') = v == v'
  (FileValue v) == (FileValue v') = v == v'
  _ == _ = False

instance Ord (FormValue ct) where
  compare = \case
    TextValue v -> \case
      TextValue v' -> compare v v'
      FileValue _ -> LT
    FileValue f -> \case
      FileValue f' -> compare f f'
      TextValue _ -> GT

instance NFData (FormValue ct) where
  rnf = \case
    TextValue v -> rnf v
    FileValue f -> rnf f

_TextValue :: Prism (FormValue anyType) (FormValue anyType) Text Text
_TextValue = prism' TextValue $ \case
  TextValue t -> Just t
  _ -> Nothing

_FileValue :: Prism' (FormValue MultipartFormData) UploadedFile
_FileValue = prism' FileValue $ \case
  FileValue f -> Just f
  _ -> Nothing

-- | Type of a full form submission in a particular content type.
type FormSubmission :: FormContentType -> Type
data FormSubmission contentType where
  -- | An empty submission - no keys or values at this nesting level.
  SubmissionEmpty :: FormSubmission contentType
  -- | A submitted value from the user.
  SubmissionValue :: !(FormValue contentType) -> FormSubmission contentType
  -- | An array of values from the user (indicated with brace characters in the submission key)
  SubmissionArray :: !(Seq.Seq (FormSubmission contentType)) -> FormSubmission contentType
  -- | An object of values from the user (indicated with dots in the submission key).
  SubmissionObject :: !(Map.Map Text (FormSubmission contentType)) -> FormSubmission contentType
  deriving (Show, Eq, Ord, Generic)

instance NFData (FormSubmission contentType)

_SubmissionEmpty :: Prism' (FormSubmission contentType) ()
_SubmissionEmpty = prism' (const SubmissionEmpty) $ \case
  SubmissionEmpty -> Just ()
  _ -> Nothing

_SubmissionValue :: Prism' (FormSubmission contentType) (FormValue contentType)
_SubmissionValue = prism' SubmissionValue $ \case
  SubmissionValue v -> Just v
  _ -> Nothing

_SubmissionArray :: Prism' (FormSubmission contentType) (Seq.Seq (FormSubmission contentType))
_SubmissionArray = prism' SubmissionArray $ \case
  SubmissionArray arr -> Just arr
  _ -> Nothing

_SubmissionObject :: Prism' (FormSubmission contentType) (Map.Map Text (FormSubmission contentType))
_SubmissionObject = prism' SubmissionObject $ \case
  SubmissionObject o -> Just o
  _ -> Nothing

ixtraverseFormValues ::
  forall contentType contentType' f.
  (Applicative f) =>
  (FormCanonicalKey -> FormValue contentType -> f (FormValue contentType')) ->
  FormSubmission contentType ->
  f (FormSubmission contentType')
ixtraverseFormValues f = go (MkFormCanonicalKey mempty)
  where
    go key = \case
      SubmissionEmpty -> pure SubmissionEmpty
      SubmissionValue v -> SubmissionValue <$> f key v
      SubmissionArray arr ->
        SubmissionArray
          <$> itraverse
            (go . appendCanonicalPiece key . CanonicalArrayPiece)
            arr
      SubmissionObject obj ->
        SubmissionObject
          <$> itraverse
            (go . appendCanonicalPiece key . CanonicalObjectPiece)
            obj

ixFormValues :: IxTraversal FormCanonicalKey (FormSubmission contentType) (FormSubmission contentType') (FormValue contentType) (FormValue contentType')
ixFormValues = itraversalVL ixtraverseFormValues

formValues :: Traversal (FormSubmission contentType) (FormSubmission contentType') (FormValue contentType) (FormValue contentType')
formValues = noIx ixFormValues

traverseFormValues ::
  (Applicative f) =>
  (FormValue contentType -> f (FormValue contentType')) ->
  FormSubmission contentType ->
  f (FormSubmission contentType')
traverseFormValues = traverseOf formValues

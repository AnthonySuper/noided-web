{-# LANGUAGE DataKinds #-}

-- | Public types for the noided-form library.
module Noided.Form.Types
  ( -- * Form Content Types
    FormContentType (..),

    -- * Form Values
    FormValue (..),
    _TextValue,
    _FileValue,

    -- * Uploaded Files
    UploadedFile (..),

    -- * Form Submissions
    FormSubmission (..),
    _SubmissionEmpty,
    _SubmissionValue,
    _SubmissionArray,
    _SubmissionObject,
    ixtraverseFormValues,
    ixFormValues,
    formValues,
    traverseFormValues,

    -- * Form Keys
    FormCanonicalKey (..),
    FormCanonicalPiece (..),
    appendCanonicalPiece,
    emptyCanonicalKey,
    FormInputKey,
    FormInputPiece (..),
  )
where

import Noided.Form.Internal.Type.FormCanonicalKey
import Noided.Form.Internal.Type.FormContentType
import Noided.Form.Internal.Type.FormInputKey
import Noided.Form.Internal.Type.FormSubmission
import Noided.Form.Internal.Type.UploadedFile

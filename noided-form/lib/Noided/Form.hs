{-# LANGUAGE DataKinds #-}

-- | Public API for the noided-form library.
--
-- This module provides a unified interface for working with form submissions in Haskell.
-- It includes types for representing forms, parsing form data, and converting form
-- submissions to Haskell types.
--
-- = Example Usage
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- > {-# LANGUAGE DataKinds #-}
-- >
-- > import Noided.Form
-- > import GHC.Generics
-- >
-- > data User = User
-- >   { name :: Text
-- >   , email :: Text
-- >   , age :: Int
-- >   } deriving (Generic, Show)
-- >
-- > instance FromFormSubmission 'UrlEncoded User
-- >
-- > -- Parse a form submission
-- > parseUser :: FormSubmission 'UrlEncoded -> Either Text User
-- > parseUser = fromFormSubmission
module Noided.Form
  ( -- * Form Types

    -- ** Content Types
    FormContentType (..),

    -- ** Form Values
    FormValue (..),
    _TextValue,
    _FileValue,

    -- ** Uploaded Files
    UploadedFile (..),

    -- ** Form Submissions
    FormSubmission (..),
    _SubmissionEmpty,
    _SubmissionValue,
    _SubmissionArray,
    _SubmissionObject,
    urlSubmissionToMultipartSubmission,
    ixtraverseFormValues,
    ixFormValues,
    formValues,
    traverseFormValues,

    -- *** Of unknown kind
    SomeFormSubmission (..),
    multipartFromSomeSubmission,

    -- ** Form Keys
    FormCanonicalKey (..),
    FormCanonicalPiece (..),
    emptyCanonicalKey,
    appendCanonicalPiece,
    canonicalKeyToFieldNameBuilder,
    canonicalKeyToFieldName,
    FormInputKey,
    FormInputPiece (..),

    -- * Parsing
    fromKeysAndValues,
    fromTextKeysAndValues,
    fromTextKeysAndValuesStrict,
    parseInputKey,

    -- * Converting from Form Submissions
    FromFormSubmission (..),
    parseAtKey,

    -- ** Via Newtypes
    ViaHttpParam (..),

    -- ** Generic Deriving
    GFromFormSubmission (..),
    gfromFormSubmission,
  )
where

import Noided.Form.FromFormSubmission
import Noided.Form.Parse
import Noided.Form.Types

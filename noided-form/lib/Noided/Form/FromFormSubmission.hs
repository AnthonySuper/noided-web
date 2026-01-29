{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Type class for converting form submissions to Haskell types.
module Noided.Form.FromFormSubmission
  ( -- * FromFormSubmission Class
    FromFormSubmission (..),
    parseAtKey,

    -- * Via Newtypes
    ViaHttpParam (..),

    -- * Generic Deriving
    GFromFormSubmission (..),
    gfromFormSubmission,
  )
where

import Noided.Form.Internal.FromFormSubmission

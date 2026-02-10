module Noided.Web.Internal.Type.ServerError where

import GHC.Exception (CallStack)
import Type.Reflection

-- | Some server error, which represents an exceptional situation
-- that the user really shouldn't be expected to handle.
data SomeServerError where
  SomeServerError ::
    (Typeable err) =>
    -- | Textual representation of the server error.
    String ->
    -- | Where the error occurred.
    Maybe CallStack ->
    -- | Actual error.
    -- We can look up this error in an actual list of responders later.
    err ->
    SomeServerError

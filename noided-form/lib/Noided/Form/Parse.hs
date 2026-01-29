-- | Parsing functions for form submissions.
module Noided.Form.Parse
  ( -- * Parsing
    fromKeysAndValues,
    fromTextKeysAndValues,
    fromTextKeysAndValuesStrict,
    parseInputKey,
  )
where

import Noided.Form.Internal.Parse

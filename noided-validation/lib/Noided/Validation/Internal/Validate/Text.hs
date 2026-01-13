module Noided.Validation.Internal.Validate.Text where

import Data.Text (Text)
import qualified Data.Text as T
import Noided.Validation.Internal.ValidationError.Text
import Noided.Validation.Internal.Validator

-- | Validate that a Text value starts with the given prefix.
startsWith :: (Monad m) => Text -> Text -> ValidatorT m ()
startsWith prefix val =
  if T.isPrefixOf prefix val
    then return ()
    else failNonfatal $ DoesNotStartWith prefix val

-- | Validate that a Text value ends with the given suffix.
endsWith :: (Monad m) => Text -> Text -> ValidatorT m ()
endsWith suffix val =
  if T.isSuffixOf suffix val
    then return ()
    else failNonfatal $ DoesNotEndWith suffix val

-- | Validate that a Text value contains the given substring.
contains :: (Monad m) => Text -> Text -> ValidatorT m ()
contains needle val =
  if T.isInfixOf needle val
    then return ()
    else failNonfatal $ DoesNotContain needle val

-- | Validate that a Text value DOES NOT contain the given substring.
notContains :: (Monad m) => Text -> Text -> ValidatorT m ()
notContains needle val =
  if T.isInfixOf needle val
    then failNonfatal $ Contains needle val
    else return ()

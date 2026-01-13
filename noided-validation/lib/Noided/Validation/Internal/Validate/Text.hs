module Noided.Validation.Internal.Validate.Text where

import Control.Monad (unless, when)
import Data.Text (Text)
import Data.Text qualified as T
import Noided.Validation.Internal.ValidationError.Text
import Noided.Validation.Internal.Validator

-- | Validate that a Text value starts with the given prefix.
startsWith :: (Monad m) => Text -> Text -> ValidatorT m ()
startsWith prefix val =
  unless
    (T.isPrefixOf prefix val)
    (failNonfatal $ DoesNotStartWith prefix)

-- | Validate that a Text value ends with the given suffix.
endsWith :: (Monad m) => Text -> Text -> ValidatorT m ()
endsWith suffix val =
  unless
    (T.isSuffixOf suffix val)
    (failNonfatal $ DoesNotEndWith suffix)

-- | Validate that a Text value contains the given substring.
contains :: (Monad m) => Text -> Text -> ValidatorT m ()
contains needle val =
  unless
    (T.isInfixOf needle val)
    (failNonfatal $ DoesNotContain needle)

-- | Validate that a Text value DOES NOT contain the given substring.
notContains :: (Monad m) => Text -> Text -> ValidatorT m ()
notContains needle val =
  when (T.isInfixOf needle val) $ failNonfatal $ Contains needle

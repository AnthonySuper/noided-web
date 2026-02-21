{-# LANGUAGE LambdaCase #-}

module OptBeer.Validate.Password where

import Data.Password.Types (mkPassword)
import Data.Password.Validate qualified as PassV
import Data.Text (pack)
import Noided.Validation
import OptBeer.Type.Hashword (OpaquePassword (..))
import OptBeer.ValidationError.PasswordComplexity qualified as Err

-- | Convert a ValidationResult from Data.Password.Validate into ValidatorT
passwordValidationResultToValidator :: (Monad m) => PassV.ValidationResult -> ValidatorT m ()
passwordValidationResultToValidator PassV.ValidPassword = return ()
passwordValidationResultToValidator (PassV.InvalidPassword reasons) = mapM_ (failNonfatal . mapReason) reasons
  where
    mapReason :: PassV.InvalidReason -> SomeValidationError
    mapReason = \case
      PassV.PasswordTooShort minLen provLen -> toSomeValidationError $ Err.PasswordTooShort minLen provLen
      PassV.PasswordTooLong maxLen provLen -> toSomeValidationError $ Err.PasswordTooLong maxLen provLen
      PassV.NotEnoughReqChars cat minAmt provAmt -> toSomeValidationError $ Err.NotEnoughReqChars (pack $ show cat) minAmt provAmt
      PassV.InvalidCharacters chars -> toSomeValidationError $ Err.InvalidCharacters chars

-- | A default password policy for the application.
-- Uses the library default (usually just a minimum length).
defaultOptBeerPasswordPolicy :: PassV.ValidPasswordPolicy
defaultOptBeerPasswordPolicy = PassV.defaultPasswordPolicy_

-- | Validate a password against the default policy.
validatePasswordComplexity :: (Monad m) => OpaquePassword -> ValidatorT m ()
validatePasswordComplexity (MkOpaquePassword pw) =
  passwordValidationResultToValidator $ PassV.validatePassword defaultOptBeerPasswordPolicy (mkPassword pw)

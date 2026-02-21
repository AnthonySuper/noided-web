{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.Form.Validate.CreateUser where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Noided.Form.HKD
import Noided.Form.Types (FormValue (..))
import Noided.Sql
import Noided.Validation
import OptBeer.DB.Table.Actor
import OptBeer.DB.Table.User
import OptBeer.Form.Type.CreateUser
import OptBeer.Type.Hashword
import OptBeer.Validate.Password
import OptBeer.ValidationError.DoesNotMatchConfirmation
import OptBeer.ValidationError.ValueTaken

-- | Validates that a user can be created.
createUserValidator :: FormValidator (TransactM e) (SubformField CreateUserF)
createUserValidator = validateBefore $ \case
  SubformInput inputs -> do
    let confirmEmailText = fieldInputToText inputs.confirmEmail.val
        confirmPasswordOpaque = fieldInputToOpaquePassword inputs.confirmPassword.val

    return $
      validateSubform $
        CreateUser
          { name = validateUserName,
            email = validateUserEmail confirmEmailText,
            confirmEmail = validateInputRaw (return . fieldInputToText),
            password = validateUserPassword confirmPasswordOpaque,
            confirmPassword = validateInputRaw (return . fieldInputToOpaquePassword)
          }

fieldInputToText :: FieldInput Text -> Text
fieldInputToText = \case
  FromTyped t -> t
  FromForm (TextValue t) -> t
  _ -> ""

fieldInputToOpaquePassword :: FieldInput OpaquePassword -> OpaquePassword
fieldInputToOpaquePassword = \case
  FromTyped t -> t
  FromForm (TextValue t) -> MkOpaquePassword t
  _ -> MkOpaquePassword ""

validateUserName :: FormValidator (TransactM e) (InputField Text)
validateUserName = validateInputRaw $ \fi -> do
  let nameText = fieldInputToText fi
  exists <- lift $ queryMaybe $ do
    row <- addFrom_ (fromBase_ actorsTable)
    addWhere_ (row.name ==. bindParam nameText)
    select_ $ Element row.name
  case exists of
    Just _ -> failNonfatal ValueTaken
    Nothing -> return ()
  return nameText

validateUserEmail :: Text -> FormValidator (TransactM e) (InputField Text)
validateUserEmail confirmEmail = validateInputRaw $ \fi -> do
  let emailText = fieldInputToText fi
  when (emailText /= confirmEmail) $
    failNonfatal DoesNotMatchConfirmation

  exists <- lift $ queryMaybe $ do
    row <- addFrom_ (fromBase_ usersTable)
    addWhere_ (row.email ==. bindParam emailText)
    select_ $ Element row.email
  case exists of
    Just _ -> failNonfatal ValueTaken
    Nothing -> return ()
  return emailText

validateUserPassword :: OpaquePassword -> FormValidator (TransactM e) (InputField OpaquePassword)
validateUserPassword confirmPassword = validateInputRaw $ \fi -> do
  let pw = fieldInputToOpaquePassword fi
  when (pw /= confirmPassword) $
    failNonfatal DoesNotMatchConfirmation
  validatePasswordComplexity pw
  return pw

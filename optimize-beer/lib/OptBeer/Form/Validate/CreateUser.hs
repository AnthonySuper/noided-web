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
import Noided.Sql
import Noided.Validation
import OptBeer.DB.Table.Actor
import OptBeer.DB.Table.User
import OptBeer.Form.Type.CreateUser
import OptBeer.Type.Hashword
import OptBeer.ValidationError.DoesNotMatchConfirmation
import OptBeer.ValidationError.ValueTaken

-- | Validates that a user can be created.
createUserValidator :: FormValidator (TransactM e) (SubformField CreateUserF)
createUserValidator = validateBefore $ \case
  SubformInput inputs -> do
    let mConfirmEmail = case inputs.confirmEmail of
          InputInput (FromTyped t) -> Just t
          _ -> Nothing
        mConfirmPassword = case inputs.confirmPassword of
          InputInput (FromTyped t) -> Just t
          _ -> Nothing

    return $
      validateSubform $
        CreateUser
          { name = validateUserName,
            email = validateUserEmail mConfirmEmail,
            confirmEmail = validateInput return,
            password = validateUserPassword mConfirmPassword,
            confirmPassword = validateInput return
          }

validateUserName :: FormValidator (TransactM e) (InputField Text)
validateUserName = validateInput $ \nameText -> do
  exists <- lift $ queryMaybe $ do
    row <- addFrom_ (fromBase_ actorsTable)
    addWhere_ (row.name ==. bindParam nameText)
    select_ $ Element row.name
  case exists of
    Just _ -> failNonfatal ValueTaken
    Nothing -> return ()
  return nameText

validateUserEmail :: Maybe Text -> FormValidator (TransactM e) (InputField Text)
validateUserEmail mConfirm = validateInput $ \emailText -> do
  when (Just emailText /= mConfirm) $
    failNonfatal DoesNotMatchConfirmation

  exists <- lift $ queryMaybe $ do
    row <- addFrom_ (fromBase_ usersTable)
    addWhere_ (row.email ==. bindParam emailText)
    select_ $ Element row.email
  case exists of
    Just _ -> failNonfatal ValueTaken
    Nothing -> return ()
  return emailText

validateUserPassword :: Maybe OpaquePassword -> FormValidator (TransactM e) (InputField OpaquePassword)
validateUserPassword mConfirm = validateInput $ \pw -> do
  when (Just pw /= mConfirm) $
    failNonfatal DoesNotMatchConfirmation
  return pw

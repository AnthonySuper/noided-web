{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module OptBeer.Form.Validate.CreateUserSpec (spec) where

import Data.Pool (Pool)
import Data.Text (Text)
import Data.Typeable (typeOf)
import Hasql.Connection (Connection)
import Noided.Form.HKD
import Noided.Row
import Noided.Sql
import Noided.Validation
import OptBeer.DB.Table.Actor
import OptBeer.DB.Table.SpecHelper
import OptBeer.DB.Table.User
import OptBeer.Form.Type.CreateUser
import OptBeer.Form.Validate.CreateUser
import OptBeer.ValidationError.DoesNotMatchConfirmation
import OptBeer.ValidationError.ValueTaken
import Optics.Core (toListOf)
import Test.Hspec

spec :: SpecWith (Pool Connection)
spec = describe "createUserValidator" $ do
  it "validates a correct form" $ \pool -> do
    let input =
          CreateUser
            { name = InputInput $ FromTyped "Alice",
              email = InputInput $ FromTyped "alice@example.com",
              confirmEmail = InputInput $ FromTyped "alice@example.com",
              password = InputInput $ FromTyped "Password123!",
              confirmPassword = InputInput $ FromTyped "Password123!"
            }
    res <- runDB @String (validateFormDB createUserValidator input) pool
    case res of
      Right _ -> return ()
      Left err -> expectationFailure $ "Expected success, got errors: " <> show err

  it "fails when email does not match confirmation" $ \pool -> do
    let input =
          CreateUser
            { name = InputInput $ FromTyped "Alice",
              email = InputInput $ FromTyped "alice@example.com",
              confirmEmail = InputInput $ FromTyped "mismatch@example.com",
              password = InputInput $ FromTyped "Password123!",
              confirmPassword = InputInput $ FromTyped "Password123!"
            }
    res <- runDB @String (validateFormDB createUserValidator input) pool
    case res of
      Right _ -> expectationFailure "Expected error, got success"
      Left err -> do
        err.innerErrors.email.innerErrors `shouldSatisfy` (`hasError` DoesNotMatchConfirmation)

  it "fails when password does not match confirmation" $ \pool -> do
    let input =
          CreateUser
            { name = InputInput $ FromTyped "Alice",
              email = InputInput $ FromTyped "alice@example.com",
              confirmEmail = InputInput $ FromTyped "alice@example.com",
              password = InputInput $ FromTyped "Password123!",
              confirmPassword = InputInput $ FromTyped "mismatch"
            }
    res <- runDB @String (validateFormDB createUserValidator input) pool
    case res of
      Right _ -> expectationFailure "Expected error, got success"
      Left err -> do
        err.innerErrors.password.innerErrors `shouldSatisfy` (`hasError` DoesNotMatchConfirmation)

  it "fails when name is taken" $ \pool -> do
    let input =
          CreateUser
            { name = InputInput $ FromTyped "TakenName",
              email = InputInput $ FromTyped "unique@example.com",
              confirmEmail = InputInput $ FromTyped "unique@example.com",
              password = InputInput $ FromTyped "Password123!",
              confirmPassword = InputInput $ FromTyped "Password123!"
            }
    res <- runDB @String (do
      -- Pre-insert a user with the same name
      let insertActor = insertReturning actorsTable (values_ [#name :==> mutateVal_ (bindParam ("TakenName" :: Text)) :::%? EmptyWrappedRow]) (\row -> Element $ row.id)
      _ <- querySingleRow insertActor
      validateFormDB createUserValidator input) pool
    case res of
      Right _ -> expectationFailure "Expected error, got success"
      Left err -> do
        err.innerErrors.name.innerErrors `shouldSatisfy` (`hasError` ValueTaken)

  it "fails when email is taken" $ \pool -> do
    let input =
          CreateUser
            { name = InputInput $ FromTyped "UniqueName",
              email = InputInput $ FromTyped "taken@example.com",
              confirmEmail = InputInput $ FromTyped "taken@example.com",
              password = InputInput $ FromTyped "Password123!",
              confirmPassword = InputInput $ FromTyped "Password123!"
            }
    res <- runDB @String (do
      -- Pre-insert a user with the same email
      let insertActor = insertReturning actorsTable (values_ [#name :==> mutateVal_ (bindParam @Text "OtherUser") :::%? EmptyWrappedRow]) (\row -> Element $ row.id)
      actorId <- querySingleRow insertActor

      let insertUser = insertReturning usersTable (values_ [#id :==> mutateVal_ (bindParam actorId) :::%? #email :==> mutateVal_ (bindParam @Text "taken@example.com") :::%? EmptyWrappedRow]) (\row -> Element $ row.id)
      _ <- querySingleRow insertUser
      validateFormDB createUserValidator input) pool
    case res of
      Right _ -> expectationFailure "Expected error, got success"
      Left err -> do
        err.innerErrors.email.innerErrors `shouldSatisfy` (`hasError` ValueTaken)

  it "fails on simple password" $ \pool -> do
    let input =
          CreateUser
            { name = InputInput $ FromTyped "Alice",
              email = InputInput $ FromTyped "alice@example.com",
              confirmEmail = InputInput $ FromTyped "alice@example.com",
              password = InputInput $ FromTyped "short",
              confirmPassword = InputInput $ FromTyped "short"
            }
    res <- runDB @String (validateFormDB createUserValidator input) pool
    case res of
      Right _ -> expectationFailure "Expected error, got success"
      Left err -> do
        -- Our default policy requires at least 8 chars
        err.innerErrors.password.innerErrors `shouldSatisfy` hasErrorOfType "PasswordTooShort"

-- | Helper to check if a set of validation errors contains an error of a certain type by name
-- This is a bit hacky because we don't have a good way to match on existential types without more boilerplate
hasErrorOfType :: String -> ValidationErrors -> Bool
hasErrorOfType typeName es = any (\(SomeValidationError e) -> show (typeOf e) == typeName) (toListOf allErrors es)

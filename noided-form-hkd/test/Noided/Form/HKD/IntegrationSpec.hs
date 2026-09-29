{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Noided.Form.HKD.IntegrationSpec (spec) where

import Data.Aeson (FromJSON, eitherDecode)
import Data.Either (isLeft)
import Data.HKD
import Data.IntMap qualified as IM
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import Noided.Form.HKD
import Noided.Validation
import Test.Hspec

-- Define custom errors
newtype TooYoung = TooYoung {tooYoungAge :: Int}
  deriving (Show, Eq, Ord, Generic)

instance ValidationError TooYoung

newtype TooLong = TooLong {tooLongText :: Text}
  deriving (Show, Eq, Ord, Generic)

instance ValidationError TooLong

-- Define Forms
data Address f = Address
  { street :: f (InputField Text),
    city :: f (InputField Text)
  }
  deriving (Generic)

deriving instance
  (Show (f (InputField Text))) => Show (Address f)

instance FFunctor Address where
  ffmap = ffmapDefault

instance FFoldable Address where
  ffoldMap = ffoldMapDefault

instance FTraversable Address where
  ftraverse = gftraverse

instance FZip Address where
  fzipWith = gfzipWith

instance FRepeat Address where
  frepeat = gfrepeat

deriving via (Generically (Address FormErrors)) instance Semigroup (Address FormErrors)

deriving via (Generically (Address FormErrors)) instance Monoid (Address FormErrors)

instance HKDForm Address

deriving anyclass instance FromJSON (Address FormInput)

data User f = User
  { name :: f (InputField Text),
    age :: f (InputField Int),
    address :: f (SubformField Address),
    tags :: f (ListField (InputField Text))
  }
  deriving (Generic)

deriving instance
  ( Show (f (InputField Text)),
    Show (f (InputField Int)),
    Show (f (SubformField Address)),
    Show (f (ListField (InputField Text)))
  ) =>
  Show (User f)

instance FFunctor User where
  ffmap = ffmapDefault

instance FFoldable User where
  ffoldMap = ffoldMapDefault

instance FTraversable User where
  ftraverse = gftraverse

instance FZip User where
  fzipWith = gfzipWith

instance FRepeat User where
  frepeat = gfrepeat

deriving via (Generically (User FormErrors)) instance Semigroup (User FormErrors)

deriving via (Generically (User FormErrors)) instance Monoid (User FormErrors)

instance HKDForm User

deriving anyclass instance FromJSON (User FormInput)

shouldHaveError :: (ValidationError p) => ValidationErrors -> p -> Expectation
errs `shouldHaveError` err =
  errs `shouldSatisfy` (`hasError` err)

data PasswordForm f = PasswordForm
  { password :: f (InputField Text),
    confirmPassword :: f (InputField Text)
  }
  deriving (Generic)

deriving instance
  (Show (f (InputField Text))) => Show (PasswordForm f)

instance FFunctor PasswordForm where ffmap = ffmapDefault

instance FFoldable PasswordForm where ffoldMap = ffoldMapDefault

instance FTraversable PasswordForm where ftraverse = gftraverse

instance FZip PasswordForm where fzipWith = gfzipWith

instance FRepeat PasswordForm where frepeat = gfrepeat

deriving via (Generically (PasswordForm FormErrors)) instance Semigroup (PasswordForm FormErrors)

deriving via (Generically (PasswordForm FormErrors)) instance Monoid (PasswordForm FormErrors)

instance HKDForm PasswordForm

data PasswordsDoNotMatch = PasswordsDoNotMatch
  deriving (Show, Eq, Ord, Generic, ValidationError)

validatePasswordForm :: (Monad m) => FormValidator m (SubformField PasswordForm)
validatePasswordForm = validateBefore $ \case
  SubformInput inputs -> do
    let pw = case inputs.password of
          InputInput (FromTyped t) -> Just t
          _ -> Nothing
        cpw = case inputs.confirmPassword of
          InputInput (FromTyped t) -> Just t
          _ -> Nothing
    check (pw == cpw) PasswordsDoNotMatch
    return $
      validateSubform $
        PasswordForm
          { password = validateInput return,
            confirmPassword = validateInput return
          }

 -- Validators

validateAddress :: (Monad m) => FormValidator m (SubformField Address)
validateAddress =
  validateSubform $
    Address
      { street = validateInput $ \t -> do
          require (not $ T.null t) Blank
          return t,
        city = validateInput $ \t -> do
          require (not $ T.null t) Blank
          return t
      }

validateUser :: (Monad m) => FormValidator m (SubformField User)
validateUser =
  validateSubform $
    User
      { name = validateInput $ \t -> do
          require (not $ T.null t) Blank
          return t,
        age = validateInput $ \i -> do
          require (i >= 18) (TooYoung i)
          return i,
        address = validateAddress,
        tags = validateList $ validateInput $ \t -> do
          check (T.length t <= 5) (TooLong t) -- non-fatal
          return t
      }

spec :: Spec
spec = describe "HKD Form Integration" $ do
  it "validates a correct form" $ do
    let input =
          User
            { name = InputInput $ FromTyped "Alice",
              age = InputInput $ FromTyped 25,
              address =
                SubformInput $
                  Address
                    { street = InputInput $ FromTyped "123 Main St",
                      city = InputInput $ FromTyped "Wonderland"
                    },
              tags = ListInput $ Seq.fromList [InputInput $ FromTyped "admin", InputInput $ FromTyped "user"]
            }
    result <- validateForm validateUser input
    case result of
      Right user -> do
        unwrap (user.name) `shouldBe` "Alice"
        unwrap (user.age) `shouldBe` 25
      Left err -> expectationFailure $ "Expected success, got errors: " ++ show err

  it "fails on invalid input (fatal)" $ do
    let input =
          User
            { name = InputInput $ FromTyped "", -- Blank
              age = InputInput $ FromTyped 10, -- TooYoung
              address =
                SubformInput $
                  Address
                    { street = InputInput $ FromTyped "123 Main St",
                      city = InputInput $ FromTyped "" -- Blank
                    },
              tags = ListInput mempty
            }
    result <- validateForm validateUser input
    case result of
      Left err -> do
        -- Check name error
        let nameErrs = err.innerErrors.name.innerErrors
        nameErrs `shouldHaveError` Blank

        -- Check age error
        let ageErrs = err.innerErrors.age.innerErrors
        ageErrs `shouldHaveError` TooYoung 10

        -- Check address city error
        let addrErrs = err.innerErrors.address.innerErrors.city.innerErrors
        addrErrs `shouldHaveError` Blank
      Right _ -> expectationFailure "Expected errors, got success"

  it "collects non-fatal errors" $ do
    let input =
          User
            { name = InputInput $ FromTyped "Bob",
              age = InputInput $ FromTyped 30,
              address =
                SubformInput $
                  Address
                    { street = InputInput $ FromTyped "St",
                      city = InputInput $ FromTyped "C"
                    },
              tags = ListInput $ Seq.fromList [InputInput $ FromTyped "toolongtag"] -- TooLong (non-fatal)
            }

    result <- validateForm validateUser input
    case result of
      Left err -> do
        let tagErrs = err.innerErrors.tags
        let innerListErrs = tagErrs.innerErrors
        case IM.lookup 0 innerListErrs of
          Just e ->
            e.innerErrors `shouldHaveError` TooLong "toolongtag"
          Nothing -> expectationFailure "Expected error at index 0"
      Right _ -> expectationFailure "Expected failure due to non-fatal error being reported"

  it "supports conditional validation with ValidateBefore" $ do
    let input =
          PasswordForm
            { password = InputInput $ FromTyped "pass123",
              confirmPassword = InputInput $ FromTyped "mismatch"
            }
    result <- validateForm validatePasswordForm input
    case result of
      Left err -> do
        err.baseErrors `shouldHaveError` PasswordsDoNotMatch
      Right _ -> expectationFailure "Expected mismatch error"

  describe "JSON input" $ do
    it "treats a missing key as NotPresent and reports it as a field error" $ do
      let Right input = eitherDecode @(Address FormInput) "{\"street\": \"Main St\"}"
      input.city `shouldBe` InputInput NotPresent
      result <- validateForm validateAddress input
      case result of
        Left err -> do
          err.innerErrors.city.innerErrors `shouldSatisfy` (not . nullErrors)
          err.innerErrors.street.innerErrors `shouldSatisfy` nullErrors
        Right _ -> expectationFailure "Expected a city error"

    it "treats null as NotPresent" $ do
      let Right input = eitherDecode @(Address FormInput) "{\"street\": null, \"city\": \"X\"}"
      input.street `shouldBe` InputInput NotPresent
      input.city `shouldBe` InputInput (FromTyped "X")

    it "treats missing and null lists as empty" $ do
      let json = "{\"name\": \"A\", \"age\": 20, \"address\": {}, \"tags\": null}"
          Right input = eitherDecode @(User FormInput) json
      input.tags `shouldBe` ListInput mempty
      let Right input' = eitherDecode @(User FormInput) "{\"name\": \"A\", \"age\": 20, \"address\": {}}"
      input'.tags `shouldBe` ListInput mempty

    it "treats a missing or null subform as an empty subform" $ do
      let Right missing = eitherDecode @(User FormInput) "{\"name\": \"A\", \"age\": 20}"
          Right nulled = eitherDecode @(User FormInput) "{\"name\": \"A\", \"age\": 20, \"address\": null}"
      missing.address.val.street `shouldBe` InputInput NotPresent
      missing.address.val.city `shouldBe` InputInput NotPresent
      nulled.address.val.city `shouldBe` InputInput NotPresent
      result <- validateForm validateUser missing
      case result of
        Left err -> do
          err.innerErrors.address.innerErrors.city.innerErrors `shouldSatisfy` (not . nullErrors)
          err.innerErrors.name.innerErrors `shouldSatisfy` nullErrors
        Right _ -> expectationFailure "Expected address errors"

    it "still fails the decode on a type mismatch" $ do
      eitherDecode @(Address FormInput) "{\"street\": 5}" `shouldSatisfy` isLeft

    it "still decodes present values as FromTyped" $ do
      let Right input = eitherDecode @(Address FormInput) "{\"street\": \"S\", \"city\": \"C\"}"
      input.street `shouldBe` InputInput (FromTyped "S")

unwrap :: FormResult (InputField a) -> a
unwrap (InputResult a) = a

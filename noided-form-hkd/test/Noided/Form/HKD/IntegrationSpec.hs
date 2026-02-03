{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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

shouldHaveError :: (ValidationError p) => ValidationErrors -> p -> Expectation
errs `shouldHaveError` err =
  errs `shouldSatisfy` (`hasError` err)

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

unwrap :: FormResult (InputField a) -> a
unwrap (InputResult a) = a

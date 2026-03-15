{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Form.HKD.Internal.Type.FormInput where

import Data.Aeson
import Data.Kind (Type)
import Data.Sequence (Seq)
import GHC.Generics
import GHC.Records
import Noided.Form.HKD.Internal.Type.HKDFieldType
import Noided.Form.Types
import Optics.Core

-- | The input given to a particular form field.
-- In order to support both actual HTTP forms and JSON with the same types,
-- this can be one of two things.
data FieldInput a
  = -- | An input value from an actual form.
    FromForm (FormValue MultipartFormData)
  | -- | An input value from some typed source, such as JSON.
    FromTyped a
  | -- | An input value was not present.
    NotPresent
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance (FromJSON a) => FromJSON (FieldInput a) where
  parseJSON = fmap FromTyped . parseJSON

_FromForm :: Prism (FieldInput a) (FieldInput a) (FormValue MultipartFormData) (FormValue MultipartFormData)
_FromForm = prism FromForm $ \case
  FromForm a -> Right a
  FromTyped a -> Left $ FromTyped a
  NotPresent -> Left NotPresent

_FromTyped :: Prism (FieldInput a) (FieldInput b) a b
_FromTyped = prism FromTyped $ \case
  FromTyped a -> Right a
  FromForm a -> Left $ FromForm a
  NotPresent -> Left NotPresent

_NotPresent :: Prism (FieldInput a) (FieldInput a) () ()
_NotPresent = prism (\() -> NotPresent) $ \case
  NotPresent -> Right ()
  FromForm a -> Left (FromForm a)
  FromTyped a -> Left (FromTyped a)

type FormInput :: HKDFieldType -> Type
data FormInput fieldType where
  -- | An input for a single form input.
  InputInput ::
    FieldInput a ->
    FormInput (InputField a)
  -- | An input for a subform, which is a subform of inputs.
  SubformInput ::
    subform FormInput ->
    FormInput (SubformField subform)
  -- | An input for a list, which is a sequence of inputs.
  ListInput ::
    Seq (FormInput input) ->
    FormInput (ListField input)

deriving instance (Show a) => Show (FormInput (InputField a))

deriving instance (Show (subform FormInput)) => Show (FormInput (SubformField subform))

deriving instance (Show (FormInput input)) => Show (FormInput (ListField input))

instance (FromJSON a) => FromJSON (FormInput (InputField a)) where
  parseJSON = fmap InputInput . parseJSON

deriving instance (Eq a) => Eq (FormInput (InputField a))

deriving instance (Eq (subform FormInput)) => Eq (FormInput (SubformField subform))

instance (FromJSON (subform FormInput)) => FromJSON (FormInput (SubformField subform)) where
  parseJSON = fmap SubformInput . parseJSON

deriving instance (Eq (FormInput input)) => Eq (FormInput (ListField input))

deriving instance (Ord a) => Ord (FormInput (InputField a))

deriving instance (Ord (subform FormInput)) => Ord (FormInput (SubformField subform))

deriving instance (Ord (FormInput input)) => Ord (FormInput (ListField input))

instance (FromJSON (FormInput input)) => FromJSON (FormInput (ListField input)) where
  parseJSON = fmap ListInput . parseJSON

_InputInput :: Iso' (FormInput (InputField a)) (FieldInput a)
_InputInput = iso (\(InputInput a) -> a) InputInput

_SubformInput :: Prism' (FormInput (SubformField subform)) (subform FormInput)
_SubformInput = prism' SubformInput $ \case
  SubformInput a -> Just a

_ListInput :: Prism' (FormInput (ListField input)) (Seq (FormInput input))
_ListInput = prism' ListInput $ \case
  ListInput a -> Just a

fieldInputFromTyped :: a -> FormInput (InputField a)
fieldInputFromTyped = InputInput . FromTyped

instance (k ~ An_Iso, a ~ FieldInput x, b ~ FieldInput x) => LabelOptic "val" k (FormInput (InputField x)) (FormInput (InputField x)) a b where
  labelOptic = iso (\(InputInput x) -> x) InputInput

instance (k ~ An_Iso, a ~ subform FormInput, b ~ subform FormInput) => LabelOptic "val" k (FormInput (SubformField subform)) (FormInput (SubformField subform)) a b where
  labelOptic = iso (\(SubformInput x) -> x) SubformInput

instance (k ~ An_Iso, a ~ Seq (FormInput input), b ~ Seq (FormInput input)) => LabelOptic "val" k (FormInput (ListField input)) (FormInput (ListField input)) a b where
  labelOptic = iso (\(ListInput x) -> x) ListInput

instance HasField "val" (FormInput (InputField x)) (FieldInput x) where
  getField (InputInput x) = x

instance HasField "val" (FormInput (SubformField subform)) (subform FormInput) where
  getField (SubformInput x) = x

instance HasField "val" (FormInput (ListField input)) (Seq (FormInput input)) where
  getField (ListInput x) = x

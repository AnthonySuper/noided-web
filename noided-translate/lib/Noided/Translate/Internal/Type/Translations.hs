{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Noided.Translate.Internal.Type.Translations where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics
import Noided.Translate.Internal.Type.Message (Message)
import Noided.Translate.Internal.Type.Messages
import Optics.At.Core
import Optics.Core

-- | A list of all translations available to the system.
-- This is a map from language codes to 'Messages'.
newtype Translations = MkTranslations {getTranslations :: Map.Map Text Messages}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving (FromJSON) via (Map.Map Text Messages)

instance Semigroup Translations where
  (MkTranslations l) <> (MkTranslations r) =
    MkTranslations $ Map.unionWith (<>) l r

instance Monoid Translations where
  mempty = MkTranslations mempty

type instance Index Translations = (Text, MessageKey)

type instance IxValue Translations = Message

instance Ixed Translations

instance At Translations where
  at (t, k) = translationsAsMap % at t % non mempty % at k

translationsAsMap :: Iso' Translations (Map.Map Text Messages)
translationsAsMap = coerced

translationsForLocaleMaybe :: Text -> Translations -> Maybe Messages
translationsForLocaleMaybe l = view $ translationsAsMap % at l
{-# INLINE translationsForLocaleMaybe #-}

translationsForLocale :: Text -> Translations -> Messages
translationsForLocale l = fromMaybe mempty . translationsForLocaleMaybe l
{-# INLINE translationsForLocale #-}

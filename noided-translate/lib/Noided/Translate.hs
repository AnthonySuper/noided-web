module Noided.Translate
  ( Translations (..),
    translationsAsMap,
    translationsForLocale,
    translationsForLocaleMaybe,
    Messages (..),
    asMessageMap,
    MessageKey (..),
    renderMessageKey,
    asTextSeq,
    parseMessageKey,
    addMessageKeyPart,
    textToMessageKey,
    Message (..),
    firstMessageMatchingFoldable,
    firstMessageMatching,
    firstMessageMatchingFoldable',
    firstMessageMatching',
    parseMessage,
    renderMessage,
    TranslateParam (..),
    AsTranslateParam (..),
    TranslateParams (..),
    GAsTranslateParams,
    gasTranslateParams,
    translateParamsMap,
  )
where

import Data.Monoid
import Noided.Translate.Internal.Render
import Noided.Translate.Internal.Type.Message
import Noided.Translate.Internal.Type.Messages
import Noided.Translate.Internal.Type.Params
import Noided.Translate.Internal.Type.Translations
import Optics.Core

firstMessageMatchingFoldable' :: (Foldable f) => Messages -> f MessageKey -> Maybe (MessageKey, Message)
firstMessageMatchingFoldable' msg = getFirst . foldMap (\x -> First $ (x,) <$> (msg ^? at x % _Just))

firstMessageMatchingFoldable :: (Foldable f) => Messages -> f MessageKey -> Maybe Message
firstMessageMatchingFoldable msg = fmap snd . firstMessageMatchingFoldable' msg

firstMessageMatching' :: Messages -> [MessageKey] -> Maybe (MessageKey, Message)
firstMessageMatching' = firstMessageMatchingFoldable'

firstMessageMatching :: Messages -> [MessageKey] -> Maybe Message
firstMessageMatching = firstMessageMatchingFoldable

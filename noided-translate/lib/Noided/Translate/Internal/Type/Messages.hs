{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Translate.Internal.Type.Messages where

import Control.Applicative
import Data.Aeson
import Data.Attoparsec.Text qualified as AT
import Data.Char (isAlphaNum)
import Data.Foldable (Foldable (foldl'))
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics
import GHC.IsList
import Noided.Translate.Internal.Type.Message
import Optics.At.Core
import Optics.Core
import Text.Read

-- | The key of a to-be-translated message.
-- This is a sequence of text, which is commonly separated with a @ . @ character.
newtype MessageKey = MessageK {getMessageKey :: Seq.Seq Text}
  deriving (Eq, Ord, Generic)
  deriving (Semigroup, Monoid) via (Seq.Seq Text)

instance Show MessageKey where
  show = show . renderMessageKey

instance Read MessageKey where
  readListPrec = readListPrecDefault
  readPrec = readPrec >>= either fail pure . parseMessageKey

renderMessageKey :: MessageKey -> Text
renderMessageKey = foldl' f mempty . getMessageKey
  where
    f :: Text -> Text -> Text
    f !existing part
      | existing == "" = part
      | otherwise = existing <> "." <> part

addMessageKeyPart :: Text -> MessageKey -> MessageKey
addMessageKeyPart t (MessageK s) = MessageK $ s Seq.:|> t
{-# INLINE addMessageKeyPart #-}

instance FromJSON MessageKey where
  parseJSON v = fromT v <|> fromA v
    where
      fromA =
        withArray "array value" $
          fmap (MessageK . Seq.fromList . toList) . traverse parseJSON
      fromT =
        withText "text value" $
          either fail pure . parseMessageKey

instance FromJSONKey MessageKey where
  fromJSONKey =
    FromJSONKeyTextParser $
      either fail pure . parseMessageKey

type instance Index MessageKey = Index (Seq.Seq Text)

type instance IxValue MessageKey = IxValue (Seq.Seq Text)

instance Ixed MessageKey where
  type IxKind MessageKey = IxKind (Seq.Seq Text)
  ix i = asTextSeq % ix i

instance IsList MessageKey where
  type Item MessageKey = Text
  fromList = MessageK . fromList
  toList = toList . getMessageKey

instance IsString MessageKey where
  fromString f = case parseMessageKey (fromString f) of
    Left _ -> mempty
    Right r -> r

asTextSeq :: Iso' MessageKey (Seq.Seq Text)
asTextSeq = iso getMessageKey MessageK

parseMessageKey :: Text -> Either String MessageKey
parseMessageKey = AT.parseOnly parseMessageKey'

parseMessageKey' :: AT.Parser MessageKey
parseMessageKey' =
  MessageK . Seq.fromList
    <$> parseMessageKeyPart `AT.sepBy1` AT.char '.'

parseMessageKeyPart :: AT.Parser Text
parseMessageKeyPart = AT.takeWhile1 isKeyPart
  where
    isKeyPart c =
      isAlphaNum c || c == '_' || c == '-'

-- | A lookup table for translations in a particular locale.
newtype Messages = MkMessages {getMessages :: Map.Map MessageKey Message}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving (Semigroup, Monoid) via (Map.Map MessageKey Message)

messagesFromKVMap :: Map.Map Text Message -> Messages
messagesFromKVMap = Map.foldMapWithKey f
  where
    f k v =
      case parseMessageKey k of
        Left _ -> mempty
        Right k' -> MkMessages $ Map.singleton k' v

type instance Index Messages = Index (Map.Map MessageKey Message)

type instance IxValue Messages = IxValue (Map.Map MessageKey Message)

instance Ixed Messages

instance At Messages where
  at v = asMessageMap % at v

instance IsList Messages where
  type Item Messages = (MessageKey, Message)
  fromList = MkMessages . fromList
  toList = toList . getMessages

-- | Used in parsing.
data MessageOrNested = Msg Message | Nested Messages
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON MessageOrNested where
  parseJSON v = (Msg <$> parseJSON v) <|> (Nested <$> parseJSON v)

-- | The JSON values of 'Messages' allow nesting. So you can define a YAML file like:
--
-- > user:
-- >   greetings:
-- >     hello: "Hello, $username!"
-- >     hello_excited: "Hello, $username!!!!!!"
--
-- This file will be parsed with the keys @ usre.grettings.hello @ and @ user.greetings.hello_excited @.
-- This lets us keep the message keys flat, while writing YAML files in a natural way.
instance FromJSON Messages where
  parseJSON v = Map.foldrWithKey mapFolder mempty <$> parseJSON v
    where
      mapFolder :: MessageKey -> MessageOrNested -> Messages -> Messages
      mapFolder k = \case
        Msg m -> at k ?~ m
        Nested m ->
          ((asMessageMap %~ Map.mapKeys (k <>)) m <>)

asMessageMap :: Iso' Messages (Map.Map MessageKey Message)
asMessageMap = coerced

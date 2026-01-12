{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Translate.Internal.Type.Message where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Attoparsec.Text qualified as AT
import Data.Char (isAlphaNum, isSpace)
import Data.Functor (($>))
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics
import Optics.Core

data PluralizationForm = One | Many
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

-- | Calculations done during formatting.
--
-- Right now you can only match on different plural forms, but in the future this will be responsible for more
-- formatting tasks, such as formatting dates and times.
data FormatCalc where
  Pluralize ::
    -- | Variable name
    Text ->
    [(PluralizationForm, Message)] ->
    Message ->
    FormatCalc
  deriving (Show, Read, Eq, Ord, Generic)

data Message where
  Fragment :: Text -> Message
  Var :: Text -> Message
  Syn :: [Message] -> Message
  Calc :: FormatCalc -> Message
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON Message where
  parseJSON =
    withText "parsed text" $
      either fail pure . AT.parseOnly parseSyn

instance IsString Message where
  fromString s = case parseMessage (fromString s) of
    Left _ -> Syn []
    Right r -> r

simplifySyn :: [Message] -> [Message]
simplifySyn = foldr f []
  where
    f (Syn m) xs = simplifySyn m ++ xs
    f (Fragment t) (Fragment t' : xs) = Fragment (t <> t') : xs
    f x xs = x : xs

lexeme :: AT.Parser a -> AT.Parser a
lexeme = (<* AT.skipMany (AT.skip isSpace))

insideSurrounding :: AT.Parser a -> AT.Parser b -> AT.Parser c -> AT.Parser c
insideSurrounding lhs rhs p = do
  _ <- lexeme lhs
  res <- p
  _ <- lexeme rhs
  return res

inParens :: AT.Parser a -> AT.Parser a
inParens = insideSurrounding (AT.char '(') (AT.char ')')

inBraces :: AT.Parser a -> AT.Parser a
inBraces = insideSurrounding (AT.char '{') (AT.char '}')

bracedMessageValue :: AT.Parser Message
bracedMessageValue = inBraces parseSyn

pluralizedMessage :: AT.Parser (PluralizationForm, Message)
pluralizedMessage = (,) <$> parseForm <*> bracedMessageValue
  where
    parseForm =
      lexeme $
        (AT.string "one" $> One)
          <|> (AT.string "many" $> Many)

parsePluralize :: AT.Parser FormatCalc
parsePluralize = do
  _ <- lexeme $ AT.string "pluralize"
  vn <- inParens $ lexeme $ parseVarName
  inBraces $ do
    msgs <- many pluralizedMessage
    defMessage <- lexeme (AT.string "default") *> bracedMessageValue
    pure $ Pluralize vn msgs defMessage

parseCalc :: AT.Parser Message
parseCalc = Calc <$> inBraces parsePluralize

parseMessage :: Text -> Either String Message
parseMessage = fmap simplify . AT.parseOnly parseSyn
  where
    simplify = _Syn %~ simplifySyn

parseSyn :: AT.Parser Message
parseSyn = Syn <$> many (parseVar <|> parseFragment <|> parseCalc)

parseVar :: AT.Parser Message
parseVar = Var <$> parseVarName

parseVarName :: AT.Parser Text
parseVarName = do
  _ <- AT.char '$'
  nc <- AT.peekChar'
  when (nc == '$') $
    fail "parse: escaped var"
  AT.takeWhile1 isAlphaNum

parseFragment :: AT.Parser Message
parseFragment = Fragment <$> (parseRawFragment <|> parseEscapedDollar)
  where
    parseRawFragment = AT.takeWhile1 (\c -> c /= '$' && c /= '}' && c /= '{')
    parseEscapedDollar = AT.string "$$" $> "$"

_Fragment :: Prism Message Message Text Text
_Fragment = prism' Fragment $ \case
  Fragment t -> Just t
  _ -> Nothing

_Var :: Prism Message Message Text Text
_Var = prism' Var $ \case
  Var f -> Just f
  _ -> Nothing

_Syn :: Prism Message Message [Message] [Message]
_Syn = prism' Syn $ \case
  Syn f -> Just f
  _ -> Nothing

messageParts :: Traversal' Message Message
messageParts = traversalVL go
  where
    goCalc :: forall f. (Applicative f) => (Message -> f Message) -> FormatCalc -> f FormatCalc
    goCalc f (Pluralize var vf m) =
      Pluralize var
        <$> traverseOf (traversed % _2) f vf
        <*> f m
    go :: forall f. (Applicative f) => (Message -> f Message) -> Message -> f Message
    go f (Calc c) = Calc <$> goCalc f c
    go f r@(Fragment _) = f r
    go f r@(Var _) = f r
    go f (Syn syns) =
      Syn <$> traverse (go f) syns

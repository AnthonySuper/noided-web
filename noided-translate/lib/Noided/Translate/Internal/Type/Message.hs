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
import Data.Text qualified as T
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
      either fail pure . parseMessage

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

-- | Where in a message we are currently parsing.
--
-- This only matters for escaping. Outside of a calculation block a @}@ carries
-- no structural meaning, so @}}@ can be read as an escaped @}@. Inside of one,
-- a @}@ always closes the enclosing block, so the escape would be ambiguous
-- (consider the @}}}@ that ends a one-armed @pluralize@) and is not offered.
data FragmentContext
  = -- | Not inside the braces of any calculation.
    TopLevel
  | -- | Inside the braces of a calculation.
    InCalc
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

bracedMessageValue :: AT.Parser Message
bracedMessageValue = inBraces $ parseSynIn InCalc

pluralizedMessage :: AT.Parser (PluralizationForm, Message)
pluralizedMessage = (,) <$> parseForm <*> bracedMessageValue
  where
    parseForm =
      lexeme $
        (AT.string "one" $> One)
          <|> (AT.string "many" $> Many)

-- | A single arm of a @pluralize@ block, before we work out which of them is
-- the @default@ one.
data PluralizeArm
  = FormArm PluralizationForm Message
  | DefaultArm Message
  deriving (Show, Read, Eq, Ord, Generic)

pluralizeArm :: AT.Parser PluralizeArm
pluralizeArm =
  (uncurry FormArm <$> pluralizedMessage)
    <|> (DefaultArm <$> (lexeme (AT.string "default") *> bracedMessageValue))

parsePluralize :: AT.Parser FormatCalc
parsePluralize = do
  _ <- lexeme $ AT.string "pluralize"
  vn <- inParens $ lexeme $ parseVarName
  inBraces $ do
    arms <- many pluralizeArm
    -- @default@ is mandatory, but it may appear in any position among the
    -- arms, not just last.
    --
    -- If it is given more than once, the *first* one wins. That matches the
    -- form arms, which 'Noided.Translate.Internal.Render.renderViaWriter'
    -- resolves with 'find', so the first matching arm wins there too.
    case [m | DefaultArm m <- arms] of
      [] -> fail "pluralize: a `default` clause is required"
      (defMessage : _) ->
        pure $ Pluralize vn [(f, m) | FormArm f m <- arms] defMessage

parseCalc :: AT.Parser Message
parseCalc = Calc <$> inBraces parsePluralize

-- | Parse an entire message, failing if any of the input is left over.
--
-- Attoparsec's 'AT.parseOnly' succeeds on a partial parse, so without this
-- check anything the parser cannot handle would silently end the message
-- early and the rest of the text would be thrown away.
parseMessage :: Text -> Either String Message
parseMessage t = do
  (msg, rest) <- AT.parseOnly ((,) <$> parseSyn <*> AT.takeText) t
  unless (T.null rest) $
    Left $
      "parse: unexpected input at character "
        <> show (T.length t - T.length rest)
        <> " of message "
        <> show t
        <> ": "
        <> show (T.take 40 rest)
  pure $ simplify msg
  where
    simplify = _Syn %~ simplifySyn

parseSyn :: AT.Parser Message
parseSyn = parseSynIn TopLevel

parseSynIn :: FragmentContext -> AT.Parser Message
parseSynIn ctx = Syn <$> many (parseVar <|> parseFragment ctx <|> parseCalc)

parseVar :: AT.Parser Message
parseVar = Var <$> parseVarName

parseVarName :: AT.Parser Text
parseVarName = do
  _ <- AT.char '$'
  nc <- AT.peekChar'
  when (nc == '$') $
    fail "parse: escaped var"
  AT.takeWhile1 isAlphaNum

parseFragment :: FragmentContext -> AT.Parser Message
parseFragment ctx = Fragment <$> (parseRawFragment <|> parseEscape)
  where
    parseRawFragment = AT.takeWhile1 (\c -> c /= '$' && c /= '}' && c /= '{')
    parseEscape =
      (AT.string "$$" $> "$")
        <|> (AT.string "{{" $> "{")
        <|> parseEscapedCloseBrace
    parseEscapedCloseBrace = case ctx of
      TopLevel -> AT.string "}}" $> "}"
      InCalc -> empty

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

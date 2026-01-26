{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Type.Syntax where

import Control.Monad.Trans.Reader
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import Data.Sequence qualified as Seq
import Data.String (IsString (..))
import Data.Text (Text)
import Noided.Sql.Internal.Class.AsBindParam

-- | A single fragment of SQL syntax.
data SyntaxFragment where
  BoundParam :: (AsBindParam a) => a -> SyntaxFragment
  RawSyntax :: Text -> SyntaxFragment

-- | Sql syntax is a function that generates a list of fragments at a particular nesting level.
newtype Syntax = Syn {runSyntax :: Word -> Seq.Seq SyntaxFragment}
  deriving (Semigroup, Monoid) via (Ap (Reader Word) (Seq.Seq SyntaxFragment))

syntaxFromText :: Text -> Syntax
syntaxFromText t = Syn $ \_ -> pure (RawSyntax t)

addNestingToSyntax :: Syntax -> Syntax
addNestingToSyntax (Syn f) = Syn $ \x -> f (x + 1)

instance IsString SyntaxFragment where
  fromString = RawSyntax . fromString

instance IsString Syntax where
  fromString s = Syn $ \_ -> pure (fromString s)

data CommaSepSyntax = Unwritten | Written Syntax

instance Semigroup CommaSepSyntax where
  Unwritten <> Unwritten = Unwritten
  Unwritten <> Written syn = Written syn
  Written syn <> Unwritten = Written syn
  Written l <> Written r = Written $ l <> ", " <> r

instance Monoid CommaSepSyntax where
  mempty = Unwritten

fromCommaSepWritten :: CommaSepSyntax -> Maybe Syntax
fromCommaSepWritten = \case
  Unwritten -> Nothing
  Written t -> Just t

fromCommaSepSyntax :: CommaSepSyntax -> Syntax
fromCommaSepSyntax = fromMaybe mempty . fromCommaSepWritten

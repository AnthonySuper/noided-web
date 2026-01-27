{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Type.Syntax where

import Control.Arrow
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.Functor.Contravariant
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import Data.Sequence qualified as Seq
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder qualified as LB
import Hasql.Encoders qualified as Enc
import Noided.Sql.Internal.Class.AsBindParam
import Optics.Core

-- | A single fragment of SQL syntax.
data SyntaxFragment where
  BoundParam :: (AsBindParam a) => a -> SyntaxFragment
  RawSyntax :: Text -> SyntaxFragment

-- | Sql syntax is a function that generates a list of fragments at a particular nesting level.
newtype Syntax = Syn {runSyntax :: Word -> Seq.Seq SyntaxFragment}
  deriving (Semigroup, Monoid) via (Ap (Reader Word) (Seq.Seq SyntaxFragment))

renderSyntaxToTextNumberedBinds :: Syntax -> Text
renderSyntaxToTextNumberedBinds = view _1 . renderSyntaxToTextWithBinds

renderSyntaxToTextWithBinds :: Syntax -> (Text, Enc.Params (), [Text])
renderSyntaxToTextWithBinds =
  flip runSyntax 0
    >>> foldMap (Ap . f)
    >>> getAp
    >>> flip runState (1, mempty, mempty)
    >>> reshuffle
  where
    reshuffle (syn, (_, params, inspected)) = (toStrict $ LB.toLazyText syn, params, reverse inspected)
    f :: SyntaxFragment -> State (Int, Enc.Params (), [Text]) LB.Builder
    f = \case
      BoundParam (bp :: boundType) -> do
        let newBind =
              case bindParamEncoder @boundType of
                EncodeNonNull v -> bp >$ Enc.param (Enc.nonNullable v)
                EncodeNullable v -> bp >$ Enc.param (Enc.nullable v)
        res <- state $ \(bindNum, boundParams, inspectedParams) ->
          (bindNum, (bindNum + 1, boundParams <> newBind, inspectBindParam bp : inspectedParams))
        pure $ LB.fromString $ "$" <> show res
      RawSyntax t -> return $ LB.fromText t

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

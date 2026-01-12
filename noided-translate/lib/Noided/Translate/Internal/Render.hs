{-# LANGUAGE OverloadedStrings #-}

module Noided.Translate.Internal.Render where

import Data.Foldable
import Data.Functor.Const
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder qualified as BT
import Noided.Translate.Internal.Type.Message
import Noided.Translate.Internal.Type.Params
import Optics.Core

renderViaWriter ::
  (Applicative f) =>
  -- | Apply formatting with the given name to a text.
  -- Currently not used, but will be in the future when we
  -- give the option to apply bold and other styles.
  (Text -> f () -> f ()) ->
  -- | Write some text
  (Text -> f ()) ->
  Message ->
  TranslateParams ->
  f ()
renderViaWriter _ writeFragment msg params = go msg
  where
    goCalc (Pluralize varName plurals m)
      | Just var <- preview (ix varName) params,
        Just pf <- paramToPluralizationForm var,
        Just (_, c) <- find (\(vf, _) -> vf == pf) plurals =
          go c
      | otherwise = go m
    go (Syn inner) = traverse_ go inner
    go (Fragment t) = writeFragment t
    go (Var f) = case preview (ix f % to shownParam) params of
      Just rendered -> writeFragment rendered
      Nothing -> writeFragment ("$" <> f)
    go (Calc c) = goCalc c

renderMessage ::
  Message ->
  TranslateParams ->
  Text
renderMessage msg params =
  toStrict $
    BT.toLazyText $
      getConst $
        renderViaWriter
          (\_ a -> a)
          (Const . BT.fromText)
          msg
          params

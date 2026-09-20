{-# LANGUAGE OverloadedStrings #-}

-- | Helpers for testing messages by what they /render to/, rather than by the
-- shape of the AST they parse into.
--
-- Asserting on the AST couples a test to details that mean nothing to a
-- translator -- whether adjacent fragments happened to be merged, say -- so
-- such a test breaks when the parser is refactored even though nothing anyone
-- can observe has changed.
module Noided.Translate.SpecHelper
  ( rendersTo,
    rendersWith,
    shouldFailToParse,
  )
where

import Data.Either (isLeft)
import Data.Text (Text)
import Noided.Translate
import Test.Hspec

-- | Parse a message, render it with no parameters, and check the result.
rendersTo :: (HasCallStack) => Text -> Text -> Expectation
rendersTo msg = rendersWith msg mempty

-- | Parse a message, render it with the given parameters, and check the result.
--
-- This goes through 'parseMessage' rather than the 'Data.String.IsString'
-- instance for 'Message' on purpose: that instance turns a parse failure into
-- an empty message, which would surface here as a baffling mismatch against
-- @""@ instead of as the parse error it is.
rendersWith :: (HasCallStack) => Text -> TranslateParams -> Text -> Expectation
rendersWith msg params expected =
  case parseMessage msg of
    Left err ->
      expectationFailure $
        "expected "
          <> show msg
          <> " to render as "
          <> show expected
          <> ", but it did not parse: "
          <> err
    Right parsed -> renderMessage parsed params `shouldBe` expected

-- | Check that a message is rejected outright.
shouldFailToParse :: (HasCallStack) => Text -> Expectation
shouldFailToParse msg = parseMessage msg `shouldSatisfy` isLeft

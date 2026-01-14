{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Noided.Form.Internal.Parse
  ( fromKeysAndValues,
    fromTextKeysAndValues,
    fromTextKeysAndValuesStrict,
    parseInputKey,
  )
where

import Control.Applicative
import Data.Attoparsec.Text qualified as AP
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable
import Data.Functor
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Noided.Form.Internal.Type.FormInputKey
import Noided.Form.Internal.Type.FormSubmission
import Optics.Core

-- | Build a new form submission from input keys and input values.
fromKeysAndValues :: (Foldable f) => f (FormInputKey, Maybe (FormValue ct)) -> FormSubmission ct
fromKeysAndValues = foldl' f SubmissionEmpty
  where
    f fc (fk, fv) = insertNewPiece fc fk fv

-- | Build a new form submission with text keys and values.
-- This applies 'parseInputKey' to each text key, and silently ignores malformed keys.
fromTextKeysAndValues :: (Foldable f) => f (Text, Maybe (FormValue ct)) -> FormSubmission ct
fromTextKeysAndValues = foldl' f SubmissionEmpty
  where
    f formSubmission (textK, val)
      | Right v <- parseInputKey textK = insertNewPiece formSubmission v val
      | otherwise = formSubmission

-- | Build a new form submission from text keys and values.
-- If any of the form input keys were malformed, return the key and the error that made it malformed.
-- Otherwise, return the built form submission.
fromTextKeysAndValuesStrict :: (Foldable f) => f (Text, Maybe (FormValue ct)) -> Either (Text, String) (FormSubmission ct)
fromTextKeysAndValuesStrict = foldl' f (pure SubmissionEmpty)
  where
    f formSubmissionOrErr (textK, val) = do
      formSubmission <- formSubmissionOrErr
      key <- first (textK,) $ parseInputKey textK
      return $ insertNewPiece formSubmission key val

asValue :: Maybe (FormValue contentType) -> FormSubmission contentType
asValue = maybe SubmissionEmpty SubmissionValue

insertNewPiece :: FormSubmission ct -> FormInputKey -> Maybe (FormValue ct) -> FormSubmission ct
insertNewPiece submission ik val =
  case (submission, ik) of
    (_, Seq.Empty) -> asValue val
    (SubmissionObject obj, TextPiece t Seq.:<| r) ->
      SubmissionObject $
        (at t % non' _SubmissionEmpty %~ (\x -> insertNewPiece x r val)) obj
    (_, TextPiece t Seq.:<| r) ->
      SubmissionObject $ Map.singleton t (insertNewPiece SubmissionEmpty r val)
    (SubmissionArray arr@(initSeq Seq.:|> obj@(SubmissionObject o)), BracesPiece Seq.:<| restKey@(TextPiece nestedKey Seq.:<| afterNested))
      | Just (SubmissionArray _) <- Map.lookup nestedKey o,
        BracesPiece Seq.:<| _ <- afterNested ->
          SubmissionArray $ initSeq Seq.:|> insertNewPiece obj restKey val
      | Map.member nestedKey o -> SubmissionArray $ arr Seq.:|> insertNewPiece SubmissionEmpty restKey val
      | otherwise -> SubmissionArray $ initSeq Seq.:|> insertNewPiece obj restKey val
    (SubmissionArray arr, BracesPiece Seq.:<| rest) ->
      SubmissionArray $ arr Seq.:|> insertNewPiece SubmissionEmpty rest val
    (_, BracesPiece Seq.:<| rest) ->
      SubmissionArray $ pure $ insertNewPiece SubmissionEmpty rest val

insideBracesParser :: AP.Parser FormInputPiece
insideBracesParser = do
  _ <- AP.char '['
  res <- AP.takeWhile1 (/= ']')
  _ <- AP.char ']'
  return $ TextPiece res

beforeBracesParser :: AP.Parser FormInputPiece
beforeBracesParser = TextPiece <$> AP.takeWhile1 (/= '[')

bracedPieceParser :: AP.Parser FormInputPiece
bracedPieceParser = AP.string "[]" $> BracesPiece

firstPieceParser :: AP.Parser FormInputPiece
firstPieceParser = beforeBracesParser <|> bracedPieceParser

inputKeyParser :: AP.Parser FormInputKey
inputKeyParser = (fullKeyParser <|> pure mempty) <* AP.endOfInput
  where
    fullKeyParser = do
      a <- firstPieceParser
      r <- AP.many' (bracedPieceParser <|> insideBracesParser)
      return $ a Seq.:<| Seq.fromList r

parseInputKey :: Text -> Either String FormInputKey
parseInputKey = AP.parseOnly inputKeyParser

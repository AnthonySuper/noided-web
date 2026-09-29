{-# LANGUAGE OverloadedStrings #-}

-- | Quoting of SQL identifiers, following the rules of Postgres's @quote_identifier@ / @fmtId@.
module Noided.Sql.Internal.Type.QuoteIdentifier
  ( quoteIdentifierIfNeeded,
    identifierNeedsQuotes,
  )
where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Noided.Sql.Internal.Type.PGKeywords (quotedKeywords)

-- | Does this identifier need double quotes?
-- It does unless it matches @[a-z_][a-z0-9_$]*@ and is not a (non-unreserved) keyword.
-- Unquoted identifiers fold to lowercase, so anything with uppercase letters must be quoted.
identifierNeedsQuotes :: Text -> Bool
identifierNeedsQuotes t =
  case T.uncons t of
    Nothing -> True
    Just (c, rest) ->
      not (isStart c && T.all isBody rest) || Set.member t quotedKeywords
  where
    isStart c = (c >= 'a' && c <= 'z') || c == '_'
    isBody c = isStart c || (c >= '0' && c <= '9') || c == '$'

-- | Wrap an identifier in double quotes (doubling embedded quotes) only if Postgres's grammar requires it.
quoteIdentifierIfNeeded :: Text -> Text
quoteIdentifierIfNeeded t
  | identifierNeedsQuotes t = "\"" <> T.replace "\"" "\"\"" t <> "\""
  | otherwise = t

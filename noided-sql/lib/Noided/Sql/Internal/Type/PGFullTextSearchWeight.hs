{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Type.PGFullTextSearchWeight where

import Noided.Sql.Internal.Type.Syntax (Syntax, syntaxFromText)

-- | Weights that can be assigned to lexemes in a @tsvector@.
data PGFullTextSearchWeight
  = WeightA -- ^ Highest weight
  | WeightB
  | WeightC
  | WeightD -- ^ Lowest weight (default)
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

pgFullTextSearchWeightSyntax :: PGFullTextSearchWeight -> Syntax
pgFullTextSearchWeightSyntax = \case
  WeightA -> syntaxFromText "A"
  WeightB -> syntaxFromText "B"
  WeightC -> syntaxFromText "C"
  WeightD -> syntaxFromText "D"

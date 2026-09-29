-- | Haskell-side values for Postgres's @tsvector@ and @tsquery@.
module Noided.Sql.Internal.Type.TSValue
  ( TSVector (..),
    TSQuery (..),
  )
where

import Data.Text (Text)
import Data.Word (Word16)
import Noided.Sql.Internal.Type.PGFullTextSearchWeight

-- | A decoded @tsvector@: each lexeme with its positions (1..16383) and their weights.
-- A position with no explicit weight has 'WeightD'.
newtype TSVector = TSVector [(Text, [(Word16, PGFullTextSearchWeight)])]
  deriving (Show, Eq)

-- | A decoded @tsquery@.
data TSQuery
  = -- | The empty query.
    TSEmpty
  | -- | A lexeme, the weights it is restricted to (empty for none), and whether it is a prefix match (@:*@).
    TSLexeme Text [PGFullTextSearchWeight] Bool
  | TSNot TSQuery
  | TSAnd TSQuery TSQuery
  | TSOr TSQuery TSQuery
  | -- | @\<N\>@ phrase operator with distance N (@\<->@ is 1).
    TSPhrase Word16 TSQuery TSQuery
  deriving (Show, Eq)

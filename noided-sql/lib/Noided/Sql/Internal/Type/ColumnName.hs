{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Type.ColumnName
  ( ColumnName (..),
    UniqueColumnName (getUniqueColumnName),
    toUniqueNames,
    UniqueNameState,
    uniquifyName,
  )
where

import Control.Monad.Trans.State.Strict
import Data.HKD
import Data.Kind
import Data.Map.Strict qualified as Map
import Data.String
import Data.Text (Text, pack)
import GHC.Generics
import Noided.Sql.Internal.Type.QuoteIdentifier (quoteIdentifierIfNeeded)

-- | Type for containing column names.
type ColumnName :: forall k. k -> Type
newtype ColumnName contained = MkColumnName {getColumnName :: Text}
  deriving (Show, Read, Eq, Ord, Generic, Functor)

instance IsString (ColumnName k) where
  fromString = MkColumnName . fromString

newtype UniqueColumnName contained = UnsafeMkUniqueColumnName {getUniqueColumnName :: Text}
  deriving (Show, Eq, Ord, Functor)

type UniqueNameState = Map.Map Text Int

-- | Pick a name that has not been handed out yet, and record it (and the base name's counter).
-- Repeated names get a @_<count>@ suffix; suffixes are skipped if the candidate is already taken,
-- so @a, a, a_1@ becomes @a, a_1, a_1_1@ rather than colliding.
uniquifyName :: Text -> UniqueNameState -> (Text, UniqueNameState)
uniquifyName name seen =
  case Map.lookup name seen of
    Nothing -> (name, Map.insert name 1 seen)
    Just start -> go start
      where
        go n =
          let candidate = name <> pack ("_" <> show n)
           in if Map.member candidate seen
                then go (n + 1)
                else (candidate, Map.insert candidate 1 (Map.insert name (n + 1) seen))

toUniqueName :: ColumnName a -> State UniqueNameState (UniqueColumnName a)
toUniqueName (MkColumnName name) = state $ \seen ->
  let (uniqueName, seen') = uniquifyName name seen
   in (UnsafeMkUniqueColumnName $ quoteIdentifierIfNeeded uniqueName, seen')

toUniqueNames :: (FTraversable hkd) => hkd ColumnName -> hkd UniqueColumnName
toUniqueNames = flip evalState mempty . ftraverse toUniqueName

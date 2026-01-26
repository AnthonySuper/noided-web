{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Type.ColumnName
  ( ColumnName (..),
    UniqueColumnName (getUniqueColumnName),
    toUniqueNames,
  )
where

import Control.Monad.Trans.State.Strict
import Data.HKD
import Data.Kind
import Data.Map.Strict qualified as Map
import Data.String
import Data.Text (Text, pack)
import GHC.Generics

-- | Type for containing column names.
type ColumnName :: forall k. k -> Type
newtype ColumnName contained = MkColumnName {getColumnName :: Text}
  deriving (Show, Read, Eq, Ord, Generic, Functor)

instance IsString (ColumnName k) where
  fromString = MkColumnName . fromString

newtype UniqueColumnName contained = UnsafeMkUniqueColumnName {getUniqueColumnName :: Text}
  deriving (Show, Eq, Ord, Functor)

type UniqueNameState = Map.Map Text Int

toUniqueName :: ColumnName a -> State UniqueNameState (UniqueColumnName a)
toUniqueName (MkColumnName name) = do
  seen <- get
  let count = Map.findWithDefault 0 name seen
  put $ Map.insert name (count + 1) seen
  let uniqueName =
        if count == 0
          then name
          else name <> pack ("_" <> show count)
  return (UnsafeMkUniqueColumnName $ "\"" <> uniqueName <> "\"")

toUniqueNames :: (FTraversable hkd) => hkd ColumnName -> hkd UniqueColumnName
toUniqueNames = flip evalState mempty . ftraverse toUniqueName

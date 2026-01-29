{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Noided.Sql.Internal.Type.TableName where

import Data.Foldable
import Data.String
import Data.Text (Text)
import GHC.Generics
import Noided.Sql.Internal.Type.QueryWriter

data TableName = NameTable {schemaName :: !(Maybe Text), tableName :: !Text}
  deriving (Show, Read, Eq, Ord, Generic)

tableNameNoSchema :: Text -> TableName
tableNameNoSchema tn = NameTable {schemaName = Nothing, tableName = tn}

instance IsString TableName where
  fromString = tableNameNoSchema . fromString

writeTableName :: TableName -> QueryWriter ()
writeTableName (NameTable sn tn) = do
  for_ sn $ \e -> do
    writeText "\""
    writeText e
    writeText "\"."
  writeText "\""
  writeText tn
  writeText "\""

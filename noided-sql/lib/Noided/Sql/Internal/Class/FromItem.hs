{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.Class.FromItem where

import Data.HKD
import Data.Kind
import Data.Text (Text)
import GHC.Generics
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.SqlExpr
import Noided.Sql.Internal.Type.SqlType
import Noided.Sql.Internal.Type.Syntax

-- | If we should use @ LATERAL @ as part of rendering a from item.
data LateralUsage
  = NeverLateral
  | SometimesLateral
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

-- | What type of column aliases to render when rendering a FROM item.
type ColumnAliasRendering :: ((SqlType -> Type) -> Type) -> Type
data ColumnAliasRendering selectList where
  NoColumnAliases :: ColumnAliasRendering selectList
  ColumnAliases :: ColumnAliasRendering selectList

class (FTraversable (FromItemSelectList item)) => FromItem item where
  type FromItemSelectList item :: (SqlType -> Type) -> Type

  -- | Alias used for the FROM item in the select list.
  -- Used to make queries more readable.
  fromItemAlias :: item -> Text
  fromItemAlias _ = "q"

  -- | If we should add a LATERAL clause to this from item, or not.
  fromItemLateralUsage :: item -> LateralUsage

  -- | If we should render column aliases when using in the SELECT list, or not.
  fromItemColumnAliases :: item -> ColumnAliasRendering (FromItemSelectList item)
  fromItemColumnAliases _ = NoColumnAliases

  -- | Write the syntax of a from item to a writer.
  writeFromItem :: item -> QueryWriter ()

  -- | Get the select list of a from item
  fromItemSelectList :: item -> FromItemSelectList item ColumnName

-- | Write the part after an AS clause for a from item.
writeFromItemAfterAs ::
  (FromItem item) =>
  item ->
  QueryWriter (FromItemSelectList item (SqlExpr scope))
writeFromItemAfterAs item = do
  tableName <- toQuotedUniqueAlias (fromItemAlias item)
  writeSyntax tableName
  let aliases = toUniqueNames (fromItemSelectList item)
  case fromItemColumnAliases item of
    NoColumnAliases -> pure ()
    ColumnAliases -> do
      "("
      let names = ffoldMap (Written . syntaxFromText . getUniqueColumnName) aliases
      writeSyntax (fromCommaSepSyntax names)
      ")"
  return $
    ffmap (\cn -> UnsafeMkSqlExpr $ tableName <> "." <> syntaxFromText (getUniqueColumnName cn)) aliases

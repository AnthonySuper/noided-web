{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Sql.Internal.HKDTableDef where

import Data.Char (isUpper, toLower)
import Data.Kind
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text qualified as Text
import GHC.Generics
import GHC.TypeLits
import Noided.Row
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.Columnar
import Noided.Sql.Internal.Type.TableDefinition
import Noided.Sql.Internal.Type.TableName

-- | Class to implement a generic, snake-cased name.
class GSnakeCasedNames (rep :: Type -> Type) where
  genericSnakeCasedNames :: rep p

instance (GSnakeCasedNames f, GSnakeCasedNames g) => GSnakeCasedNames (f :*: g) where
  genericSnakeCasedNames = genericSnakeCasedNames :*: genericSnakeCasedNames

instance (GSnakeCasedNames f) => GSnakeCasedNames (M1 D c f) where
  genericSnakeCasedNames = M1 genericSnakeCasedNames

instance (GSnakeCasedNames f) => GSnakeCasedNames (M1 C c f) where
  genericSnakeCasedNames = M1 genericSnakeCasedNames

instance (KnownSymbol s) => GSnakeCasedNames (M1 S ('MetaSel ('Just s) i1 i2 i3) (K1 R (ColumnName k))) where
  genericSnakeCasedNames = M1 $ K1 $ fromString $ Text.unpack $ camelToSnake $ Text.pack $ symbolVal (Proxy @s)

instance {-# OVERLAPPABLE #-} (Generic a, GSnakeCasedNames (Rep a)) => GSnakeCasedNames (M1 S ('MetaSel ('Just s) i1 i2 i3) (K1 R a)) where
  genericSnakeCasedNames = M1 $ K1 $ to (genericSnakeCasedNames @(Rep a))

type HKDTableDef ::
  (forall arg. ColumnUsage arg -> arg -> Type) ->
  Type
type HKDTableDef hkd =
  TableDefinition (HKDRowLabels (hkd InTableDef)) (hkd InQuery)

-- | Define a table using an HKD structure.
-- This automatically generates snake_cased column names from the record field names.
-- It also supports nested HKD structures, where nested field names are also snake_cased.
--
-- Example:
--
-- > data UserF realm f = User
-- >   { userId :: Columnar (Column AlwaysDefault NonNull Int64) realm f,
-- >     userName :: Columnar (Column NoDefault NonNull Text) realm f
-- >   }
-- >   deriving (Generic)
-- >
-- > $(defineHKDTable ''UserF)
-- >
-- > usersTable :: TableDefinition (HKDRowLabels UserTableDef) UserInQuery
-- > usersTable = hkdTableDef "users"
--
-- This will result in a table definition where the columns are named @"user_id"@ and @"user_name"@.
hkdTableDef ::
  forall (hkdTable :: forall arg. ColumnUsage arg -> arg -> Type).
  ( Generic (hkdTable InQuery ColumnName),
    GSnakeCasedNames (Rep (hkdTable InQuery ColumnName)),
    Generic (hkdTable InTableDef ColumnName),
    GSnakeCasedNames (Rep (hkdTable InTableDef ColumnName)),
    HKDToRow (hkdTable InTableDef)
  ) =>
  TableName ->
  HKDTableDef hkdTable
hkdTableDef tn =
  DefineTable
    { tableName = tn,
      columnNames = hkdToRow (to (genericSnakeCasedNames @(Rep (hkdTable InTableDef ColumnName))) :: hkdTable InTableDef ColumnName),
      selectedNames = to (genericSnakeCasedNames @(Rep (hkdTable InQuery ColumnName)))
    }

-- | Convert camelCase to snake_case.
camelToSnake :: Text.Text -> Text.Text
camelToSnake = Text.pack . go . Text.unpack
  where
    go [] = []
    go (x : xs) = toLower x : go' xs

    go' [] = []
    go' (x : xs)
      | isUpper x = '_' : toLower x : go' xs
      | otherwise = x : go' xs

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Noided.Sql.Internal.SnakeCase
-- Description: Generic snake_cased column names for table and view HKDs.
module Noided.Sql.Internal.SnakeCase
  ( GSnakeCasedNames (..),
    camelToSnake,
  )
where

import Data.Char (isUpper, toLower)
import Data.Kind
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text qualified as Text
import GHC.Generics
import GHC.TypeLits
import Noided.Sql.Internal.Type.ColumnName

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

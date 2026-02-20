{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Class.PGType where

import Data.Int
import Data.Proxy
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time
import Data.UUID (UUID)
import Noided.Sql.Internal.Type.PGArray
import Noided.Sql.Internal.Type.SqlType

-- | Types that map to a particular Postgres type.
class PGType t where
  pgTypeName :: proxy t -> Text

-- | Helper: provides the name for an array element.
class PGArrayElement (t :: SqlType) where
  pgArrayElementName :: proxy t -> Text

instance (PGType pgt) => PGArrayElement (SqlT n pgt) where
  pgArrayElementName _ = pgTypeName (Proxy @pgt)

instance (PGArrayElement pgt) => PGArrayElement (SqlT n (PGArray pgt)) where
  pgArrayElementName _ = pgArrayElementName (Proxy @pgt) <> "[]"

instance
  (PGArrayElement sqlT) =>
  PGType (PGArray sqlT)
  where
  pgTypeName _ = pgArrayElementName (Proxy @sqlT) <> "[]"

instance PGType Scientific where
  pgTypeName _ = "numeric"

instance PGType Bool where
  pgTypeName _ = "bool"

instance PGType Text where
  pgTypeName _ = "text"

instance PGType String where
  pgTypeName _ = "text"

instance PGType Int16 where
  pgTypeName _ = "int2"

instance PGType Int32 where
  pgTypeName _ = "int4"

instance PGType Int64 where
  pgTypeName _ = "int8"

instance PGType Int where
  pgTypeName _ = "int8"

instance PGType Float where
  pgTypeName _ = "float4"

instance PGType Double where
  pgTypeName _ = "float8"

instance PGType UUID where
  pgTypeName _ = "uuid"

instance PGType Day where
  pgTypeName _ = "date"

instance PGType TimeOfDay where
  pgTypeName _ = "time"

instance PGType UTCTime where
  pgTypeName _ = "timestamptz"

instance PGType LocalTime where
  pgTypeName _ = "timestamp"

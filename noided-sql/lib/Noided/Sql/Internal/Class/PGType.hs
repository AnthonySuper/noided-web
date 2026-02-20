{-# LANGUAGE OverloadedStrings #-}

module Noided.Sql.Internal.Class.PGType where

import Data.Int
import Data.Proxy
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time
import Data.UUID (UUID)
import Noided.Sql.Internal.Type.PGArray

-- | Types that map to a particular Postgres type.
class PGType t where
  pgTypeName :: proxy t -> Text

-- | Helper: provides the type of an element in a PG array.
-- We use overlapping instances to get better resolution here.
class PGArrayElement t where
  pgArrayElementName :: proxy t -> Text

instance (PGType t) => PGArrayElement t where
  pgArrayElementName = pgTypeName

instance {-# OVERLAPPING #-} (PGArrayElement t) => PGArrayElement (PGArray t) where
  pgArrayElementName _ = pgArrayElementName (Proxy @t)

instance
  (PGArrayElement elm) =>
  PGType (PGArray elm)
  where
  pgTypeName _ = pgArrayElementName (Proxy @elm) <> "[]"

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

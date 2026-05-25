{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Noided.Sql.Internal.Type.PGRow where

import Data.HKD
import Data.Kind (Type)
import Data.Typeable
import Hasql.Decoders qualified as Dec
import Noided.Sql.Internal.Class.AsHaskellValue
import Noided.Sql.Internal.Class.DecodeSelectList
import Noided.Sql.Internal.Class.PGType
import Noided.Sql.Internal.Class.UnwrapSelectList
import Noided.Sql.Internal.Type.PGDecoder
import Noided.Sql.Internal.Type.SqlType

data PGRow (contained :: (SqlType -> Type) -> Type)

instance
  ( DecodeSelectList sl,
    Typeable sl,
    UnwrapSelectList sl,
    FTraversable sl,
    Typeable (SelectListUnwrapped sl)
  ) =>
  AsHaskellValue (PGRow sl)
  where
  type HaskellTypeOf (PGRow sl) = SelectListUnwrapped sl
  decodeHaskellValue Proxy = unwrapSelectList <$> Dec.record traved
    where
      traved = ftraverse decoderToComposite slDec
      slDec = selectListDecoder @sl

instance PGType (PGRow anything) where
  pgTypeName _ = "record"

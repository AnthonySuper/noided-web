{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}

module Noided.Pathname.Internal.RoutingCapture where

import Control.DeepSeq (NFData)
import Data.GADT.Compare
import Data.GADT.Show
import Data.Kind (Type)
import Data.Text
import Data.Type.Equality
import Noided.Pathname.Internal.PieceTemplate
import Type.Reflection
import Web.HttpApiData

newtype RoutingCapture (t :: Type) = MkRoutingCapture {getRoutingCapture :: PieceTemplate (Just t)}
  deriving (Show, Read, Eq, Ord)
  deriving newtype (NFData)

pattern RouteCap :: () => (ToHttpApiData a, FromHttpApiData a, Typeable a) => RoutingCapture a
pattern RouteCap = MkRoutingCapture CapPiece

{-# COMPLETE RouteCap #-}

parseWithCapture :: RoutingCapture t -> Text -> Maybe t
parseWithCapture (MkRoutingCapture CapPiece) = parseUrlPieceMaybe

instance TestEquality RoutingCapture where
  testEquality (MkRoutingCapture a) (MkRoutingCapture b) = do
    Refl <- testEquality a b
    Just Refl

instance GShow RoutingCapture where
  gshowsPrec = defaultGshowsPrec

instance GEq RoutingCapture where
  geq (MkRoutingCapture l) (MkRoutingCapture r) = do
    Refl <- geq l r
    Just Refl

instance GCompare RoutingCapture where
  gcompare (MkRoutingCapture l) (MkRoutingCapture r) =
    case gcompare l r of
      GLT -> GLT
      GEQ -> GEQ
      GGT -> GGT

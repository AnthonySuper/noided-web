{-# LANGUAGE LambdaCase #-}

module Noided.Pathname.Internal.RoutingKey where

import Control.DeepSeq
import Control.Monad (guard)
import Data.GADT.Compare
import Data.Hashable
import Data.Kind
import Data.Text
import Data.Type.Equality
import Text.Read
import Type.Reflection
import Web.HttpApiData

data RouteCapture cap where
  RouteCap ::
    (FromHttpApiData a, Typeable a, ToHttpApiData a) =>
    RouteCapture a

type KnownRouteCapture cap = (Typeable cap, FromHttpApiData cap, ToHttpApiData cap)

knownRouteCapture :: forall cap. (KnownRouteCapture cap) => RouteCapture cap
knownRouteCapture = RouteCap

deriving instance (Show (RouteCapture cap))

instance (KnownRouteCapture cap) => Read (RouteCapture cap) where
  readListPrec = readListPrecDefault
  readPrec = parens $ prec 10 $ do
    Ident "RouteCap" <- lexP
    pure RouteCap

instance Eq (RouteCapture a) where
  _ == _ = True

instance Ord (RouteCapture a) where
  compare _ _ = EQ

instance TestEquality RouteCapture where
  testEquality (RouteCap @a) (RouteCap @b) = do
    Refl <- testEquality (typeRep @a) (typeRep @b)
    pure Refl

instance GEq RouteCapture where
  lhs `geq` rhs = testEquality lhs rhs

instance GCompare RouteCapture where
  gcompare (RouteCap @a) (RouteCap @b) =
    case gcompare (typeRep @a) (typeRep @b) of
      GLT -> GLT
      GEQ -> GEQ
      GGT -> GGT

instance NFData (RouteCapture a) where
  rnf = rwhnf

instance Hashable (RouteCapture a) where
  hash RouteCap = hash (typeRep @a)
  hashWithSalt salt RouteCap = hashWithSalt salt (typeRep @a)

useRouteCapture :: forall cap. RouteCapture cap -> Text -> Maybe cap
useRouteCapture RouteCap = parseUrlPieceMaybe @cap

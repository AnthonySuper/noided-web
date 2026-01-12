{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Noided.Pathname.Internal.PieceTemplate where

import Control.DeepSeq (NFData (..))
import Control.Monad (guard)
import Data.GADT.Compare
import Data.GADT.Show
import Data.Kind (Type)
import Data.String
import Data.Text (Text)
import Data.Type.Equality
import Text.Read
import Type.Reflection
import Web.HttpApiData

-- | A template for a single piece of a URL
data PieceTemplate (a :: Maybe Type) where
  -- | A capturing piece, with some type.
  -- We must be able to parse and serialize this type to and from URL parameters, as well as determine at runtime
  -- what type it has. The constraints in the constructor ensure that is maintained.
  CapPiece :: (ToHttpApiData a, FromHttpApiData a, Typeable a) => PieceTemplate (Just a)
  -- | A static piece.
  -- Matches a given string of text, that's it.
  StaticPiece :: Text -> PieceTemplate Nothing

deriving instance Show (PieceTemplate a)

instance GShow PieceTemplate where
  gshowsPrec = defaultGshowsPrec

instance Read (PieceTemplate Nothing) where
  readListPrec = readListPrecDefault
  readPrec = parens $ prec 10 $ do
    Ident "StaticPiece" <- lexP
    StaticPiece <$> step readPrec

instance
  ( ToHttpApiData a,
    FromHttpApiData a,
    Typeable a
  ) =>
  Read (PieceTemplate (Just a))
  where
  readListPrec = readListPrecDefault
  readPrec = do
    Ident "CapPiece" <- lexP
    pure CapPiece

instance Eq (PieceTemplate a) where
  (==) = defaultEq

instance Ord (PieceTemplate a) where
  compare = defaultCompare

instance TestEquality PieceTemplate where
  testEquality (CapPiece @l) (CapPiece @r) = do
    Refl <- testEquality (typeRep @l) (typeRep @r)
    pure Refl
  testEquality CapPiece (StaticPiece _) = Nothing
  testEquality (StaticPiece _) CapPiece = Nothing
  testEquality (StaticPiece _) (StaticPiece _) = pure Refl

instance GEq PieceTemplate where
  geq (CapPiece @l) (CapPiece @r) = do
    Refl <- geq (typeRep @l) (typeRep @r)
    pure Refl
  geq CapPiece (StaticPiece _) = Nothing
  geq (StaticPiece _) CapPiece = Nothing
  geq (StaticPiece l) (StaticPiece r) = do
    guard $ l == r
    pure Refl

instance GCompare PieceTemplate where
  gcompare (CapPiece @l) (CapPiece @r) =
    case gcompare (typeRep @l) (typeRep @r) of
      GLT -> GLT
      GEQ -> GEQ
      GGT -> GGT
  gcompare CapPiece (StaticPiece _) = GLT
  gcompare (StaticPiece _) CapPiece = GGT
  gcompare (StaticPiece l) (StaticPiece r) =
    case compare l r of
      LT -> GLT
      EQ -> GEQ
      GT -> GGT

instance (k ~ Nothing) => IsString (PieceTemplate k) where
  fromString = StaticPiece . fromString

type KnownCapture cap = (ToHttpApiData cap, FromHttpApiData cap, Typeable cap)

capPiece :: forall t. (ToHttpApiData t, FromHttpApiData t, Typeable t) => PieceTemplate (Just t)
capPiece = CapPiece

staticPiece :: Text -> PieceTemplate Nothing
staticPiece = StaticPiece

instance NFData (PieceTemplate a) where
  rnf CapPiece = ()
  rnf (StaticPiece t) = rnf t

type family PieceCaptured (inner :: Maybe Type) where
  PieceCaptured (Just t) = t
  PieceCaptured Nothing = ()

matchPieceTemplate :: PieceTemplate inner -> Text -> Maybe (PieceCaptured inner)
matchPieceTemplate (CapPiece @a) t = parseUrlPieceMaybe @a t
matchPieceTemplate (StaticPiece f) t =
  if f == t then Just () else Nothing

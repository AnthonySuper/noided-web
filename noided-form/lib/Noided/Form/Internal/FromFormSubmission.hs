{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Form.Internal.FromFormSubmission where

import Data.Int
import Data.Kind (Constraint, Type)
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack)
import Data.Text.Lazy qualified as LT
import Data.Time
import Data.UUID.Types (UUID)
import Data.Word
import GHC.Generics
import Noided.Form.Internal.Type.FormContentType
import Noided.Form.Internal.Type.FormSubmission
import Web.HttpApiData

type FromFormSubmission :: FormContentType -> Type -> Constraint
class FromFormSubmission ct a where
  fromFormSubmission :: FormSubmission ct -> Either Text a

  default fromFormSubmission ::
    (Generic a, GFromFormSubmission ct (Rep a)) =>
    FormSubmission ct ->
    Either Text a
  fromFormSubmission = gfromFormSubmission

-- | Newtype wrapper used with @ -XDerivingVia @ to derive instances of
-- 'FromFormSubmission' for types with an instance of 'FromHttpApiData'.
newtype ViaHttpParam a = ViaHttpParam {getViaHttpParam :: a}
  deriving (Show, Read, Eq, Ord, Generic)

parseAtKey' :: Text -> (FormSubmission contentType -> t) -> FormSubmission contentType -> t
parseAtKey' k f = \case
  SubmissionObject m
    | Just val <- Map.lookup k m -> f val
  _ -> f SubmissionEmpty

-- | Parse underneath a specified key.
parseAtKey :: (FromFormSubmission contentType a) => Text -> FormSubmission contentType -> Either Text a
parseAtKey t = parseAtKey' t fromFormSubmission

instance (FromFormSubmission ct a) => FromFormSubmission ct (Maybe a) where
  fromFormSubmission cs
    | SubmissionEmpty <- cs = Right Nothing
    | otherwise = Just <$> fromFormSubmission cs

instance (FromHttpApiData a) => FromFormSubmission ct (ViaHttpParam a) where
  fromFormSubmission =
    fmap ViaHttpParam . \case
      SubmissionValue (TextValue v) -> parseUrlPiece v
      _ -> Left "Expected text input value"

deriving via ViaHttpParam Bool instance FromFormSubmission ct Bool

deriving via ViaHttpParam Char instance FromFormSubmission ct Char

deriving via ViaHttpParam Double instance FromFormSubmission ct Double

deriving via ViaHttpParam Float instance FromFormSubmission ct Float

deriving via ViaHttpParam Int instance FromFormSubmission ct Int

deriving via ViaHttpParam Int8 instance FromFormSubmission ct Int8

deriving via ViaHttpParam Int16 instance FromFormSubmission ct Int16

deriving via ViaHttpParam Int32 instance FromFormSubmission ct Int32

deriving via ViaHttpParam Int64 instance FromFormSubmission ct Int64

deriving via ViaHttpParam Integer instance FromFormSubmission ct Integer

deriving via ViaHttpParam Word instance FromFormSubmission ct Word

deriving via ViaHttpParam Word8 instance FromFormSubmission ct Word8

deriving via ViaHttpParam Word16 instance FromFormSubmission ct Word16

deriving via ViaHttpParam Word32 instance FromFormSubmission ct Word32

deriving via ViaHttpParam Word64 instance FromFormSubmission ct Word64

deriving via ViaHttpParam String instance FromFormSubmission ct String

deriving via ViaHttpParam Text instance FromFormSubmission ct Text

deriving via ViaHttpParam LT.Text instance FromFormSubmission ct LT.Text

deriving via ViaHttpParam UUID instance FromFormSubmission ct UUID

deriving via ViaHttpParam Day instance FromFormSubmission ct Day

deriving via ViaHttpParam LocalTime instance FromFormSubmission ct LocalTime

deriving via ViaHttpParam ZonedTime instance FromFormSubmission ct ZonedTime

deriving via ViaHttpParam UTCTime instance FromFormSubmission ct UTCTime

-- | Generic-deriving for form submissions.
class GFromFormSubmission ct rep where
  genericFromFormSubmission :: FormSubmission ct -> Either Text (rep a)

instance (GFromFormSubmission ct f) => GFromFormSubmission ct (M1 D c f) where
  genericFromFormSubmission = fmap M1 . genericFromFormSubmission

instance (GFromFormSubmission ct f) => GFromFormSubmission ct (M1 C c f) where
  genericFromFormSubmission = fmap M1 . genericFromFormSubmission

instance (GFromFormSubmission ct f, GFromFormSubmission ct g) => GFromFormSubmission ct (f :*: g) where
  genericFromFormSubmission s = (:*:) <$> genericFromFormSubmission s <*> genericFromFormSubmission s

instance (Selector s, FromFormSubmission ct c) => GFromFormSubmission ct (M1 S s (K1 i c)) where
  genericFromFormSubmission s =
    let key = pack $ selName (undefined :: M1 S s (K1 i c) p)
     in M1 . K1 <$> parseAtKey key s

instance GFromFormSubmission ct U1 where
  genericFromFormSubmission _ = pure U1

gfromFormSubmission ::
  (Generic a, GFromFormSubmission ct (Rep a)) =>
  FormSubmission ct ->
  Either Text a
gfromFormSubmission = fmap to . genericFromFormSubmission @_ @_ @()

instance
  (Generic a, GFromFormSubmission ct (Rep a)) =>
  FromFormSubmission ct (Generically a)
  where
  fromFormSubmission = fmap Generically . gfromFormSubmission

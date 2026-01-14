{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Noided.Form.Internal.FromFormSubmission where

import Data.Int
import Data.Kind (Constraint, Type)
import Data.Text (Text)
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

-- | Newtype wrapper used with @ -XDerivingVia @ to derive instances of
-- 'FromFormSubmission' for types with an instance of 'FromHttpApiData'.
newtype ViaHttpParam a = ViaHttpParam {getViaHttpParam :: a}
  deriving (Show, Read, Eq, Ord, Generic)

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

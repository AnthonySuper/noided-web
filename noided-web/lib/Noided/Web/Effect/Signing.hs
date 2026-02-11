module Noided.Web.Effect.Signing
  ( Signing,
    verifyMessage,
    verifyMessageRotation,
    signSignableMessage,
    signMessageExpPurpose,

    -- * Obtaining signers
    mainSigner,
    fallbackSigners,

    -- * Running the effect
    runWithMainSignerAndFallbacks,
    runWithSingleSigner,
    RotationStatus (..),

    -- * Re-Exports
    SignedMessageFailure (..),
    SM.SignableMessage (..),
    SignedValue,
    getSignedValue,
  )
where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Effectful
import GHC.Generics
import Noided.Web.Internal.Effect.CurrentTime
import Noided.Web.Internal.Effect.Signing
import Noided.Web.Internal.Type.SignedMessage (SignedMessageFailure (..), SignedValue, getSignedValue)
import Noided.Web.Internal.Type.SignedMessage qualified as SM
import Optics.Core

data RotationStatus a
  = -- | Uses the current main signer
    UsesMain a
  | -- | Uses a non-primary signer
    NeedsRotation a
  deriving (Show, Read, Eq, Ord, Generic)

ignoreRotation :: RotationStatus a -> a
ignoreRotation (UsesMain a) = a
ignoreRotation (NeedsRotation a) = a

-- | Sign a signable message with the environment's main signer.
signSignableMessage :: (ToJSON a, Signing :> es) => SM.SignableMessage a -> Eff es Text
signSignableMessage r = SM.signSignableMessage <$> mainSigner <*> pure r

signMessageExpPurpose :: (Signing :> es, ToJSON a) => UTCTime -> Text -> a -> Eff es Text
signMessageExpPurpose exp' purpose msg = do
  signer <- mainSigner
  return $
    SM.signMessageExpPurpose signer exp' purpose msg

-- | Verify a message with the environment's signers.
verifyMessage :: (CurrentTime :> es, Signing :> es, FromJSON a) => Maybe Text -> Text -> Eff es (Either SignedMessageFailure (SignedValue a))
verifyMessage purpose message =
  over _Right ignoreRotation
    <$> verifyMessageRotation purpose message

verifyMessageRotation ::
  ( CurrentTime :> es,
    Signing :> es,
    FromJSON a
  ) =>
  Maybe Text ->
  Text ->
  Eff es (Either SignedMessageFailure (RotationStatus (SignedValue a)))
verifyMessageRotation purpose message = do
  ct <- getCurrentTime
  ms <- mainSigner
  case SM.verifyMessage ms ct purpose message of
    Right sv -> return (Right $ UsesMain sv)
    Left err -> do
      otherSigners <- fallbackSigners
      let possibles = [SM.verifyMessage si ct purpose message | si <- otherSigners]
      let r = toListOf (folded % _Right) possibles
      return $
        case r of
          (v : _) -> Right $ NeedsRotation v
          [] -> Left err

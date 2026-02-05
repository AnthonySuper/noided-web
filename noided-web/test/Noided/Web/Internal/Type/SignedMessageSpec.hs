module Noided.Web.Internal.Type.SignedMessageSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Base64.URL qualified as B64
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time
import Noided.Web.Internal.Type.SignedMessage
import Test.Hspec

failureOf :: SignedMessageFailure -> Either SignedMessageFailure (SignedValue Text)
failureOf = Left

spec :: Spec
spec = do
  describe "SignedMessage" $ do
    let key = "secret-key" :: ByteString
    let signer = signerHMACSHA512 key
    let msg = "hello world" :: Text
    let purpose = "test-purpose" :: Text

    it "can sign and verify a message" $ do
      now <- getCurrentTime
      let signed = signMessagePurpose signer purpose msg
      let result = verifyMessage signer now (Just purpose) signed
      getSignedValue <$> result `shouldBe` Right msg

    it "fails if the signature is tampered with" $ do
      now <- getCurrentTime
      let signed = signMessagePurpose signer purpose msg
      -- tamper with the last character
      let tampered = Text.init signed <> "AA"
      let result = verifyMessage signer now (Just purpose) tampered :: Either SignedMessageFailure (SignedValue Text)
      result `shouldSatisfy` isLeft

    it "fails if the payload is tampered with" $ do
      now <- getCurrentTime
      let signed = signMessagePurpose signer purpose msg
      -- Split, change payload, rejoin
      let (_, sig) = Text.breakOnEnd "." signed
      -- Now, replace the payload with some malicious value:
      let newPayload = B64.encode (toStrict $ Aeson.encode ("not-hello-world" :: Text))
      let tampered = decodeUtf8Lenient newPayload <> "." <> sig
      let result = verifyMessage signer now (Just purpose) tampered :: Either SignedMessageFailure (SignedValue Text)
      -- It might fail with SignatureDidNotMatch or MessageNotJSON depending on what we passed
      -- But definitely not Right
      result `shouldBe` failureOf SignatureDidNotMatch

    it "fails if expired" $ do
      now <- getCurrentTime
      let past = addUTCTime (-100) now
      let signed = signMessageExpPurpose signer past purpose msg
      let result = verifyMessage signer now (Just purpose) signed
      result `shouldBe` failureOf MessageExpired

    it "fails if purpose mismatch" $ do
      now <- getCurrentTime
      let signed = signMessagePurpose signer "purpose-A" msg
      let result = verifyMessage signer now (Just "purpose-B") signed
      result `shouldBe` failureOf PurposeDidNotMatch

    it "succeeds if expiration is in future" $ do
      now <- getCurrentTime
      let future = addUTCTime 100 now
      let signed = signMessageExpPurpose signer future purpose msg
      let result = verifyMessage signer now (Just purpose) signed
      getSignedValue <$> result `shouldBe` Right msg

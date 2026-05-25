{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Noided.Sql.Internal.RoundtripSpec (spec) where

import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific (Scientific)
import Data.UUID (UUID)
import Data.Time
import Data.Time.Clock.POSIX
import Data.IP (IPRange)
import PostgreSQL.Binary.Range (Range)
import Data.Vector (Vector)
import Noided.Sql
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Data.Pool (Pool, withResource)
import Hasql.Connection (Connection)
import qualified Noided.Sql.Internal.Roundtrip.SpecHelper as Helper

roundtrip :: forall t. (AsBindParam t, BoundNullability t ~ NonNull, AsHaskellValue (BoundType t), HaskellTypeOf (BoundType t) ~ t, Eq t, Show t) => Pool Connection -> t -> Expectation
roundtrip pool val = withResource pool $ \conn -> do
  let query = querySingleRow $ select_ $ Element (bindParam val)
  res <- transactDryRun noStatementCallback query conn :: IO (TransactionResult String t)
  case res of
    TransactOK got -> got `shouldBe` val
    TransactErr e -> expectationFailure $ "Transaction error: " <> show e
    SessionErr e -> expectationFailure $ "Session error: " <> show e

spec :: Spec
spec = Helper.hook $ do
  describe "Type Roundtripping" $ modifyMaxSuccess (const 10) $ do
    it "roundtrips Int16" $ \pool -> property $ \(val :: Int16) -> roundtrip pool val
    it "roundtrips Int32" $ \pool -> property $ \(val :: Int32) -> roundtrip pool val
    it "roundtrips Int64" $ \pool -> property $ \(val :: Int64) -> roundtrip pool val
    it "roundtrips Text" $ \pool -> property $ \(val :: Text) -> 
      let cleanVal = T.filter (/= '\NUL') val
      in roundtrip pool cleanVal
    it "roundtrips Bool" $ \pool -> property $ \(val :: Bool) -> roundtrip pool val
    it "roundtrips UTCTime" $ \pool -> property $ \(val :: UTCTime) -> 
      let -- Postgres has microsecond precision
          rounded = posixSecondsToUTCTime $ fromIntegral (floor (utcTimeToPOSIXSeconds val * 1000000) :: Integer) / 1000000
      in roundtrip pool rounded
    it "roundtrips Day" $ \pool -> property $ \(val :: Day) -> roundtrip pool val
    it "roundtrips UUID" $ \pool -> property $ \(val :: UUID) -> roundtrip pool val
    it "roundtrips Scientific" $ \pool -> property $ \(val :: Scientific) -> roundtrip pool val
    it "roundtrips Double" $ \pool -> property $ \(val :: Double) -> roundtrip pool val
    it "roundtrips Float" $ \pool -> property $ \(val :: Float) -> roundtrip pool val
    it "roundtrips Vector Int32" $ \pool -> property $ \(val :: Vector Int32) -> roundtrip pool val


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Noided.Sql.Internal.HKDViewDefSpec (spec) where

import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics
import Noided.Sql.Define
import Noided.Sql.Internal.Type.ColumnName (ColumnName (..))
import Test.Hspec

data UserSummaryF realm f = UserSummary
  { userId :: ViewColumnar (NonNullT Int64) realm f,
    userName :: ViewColumnar (NonNullT Text) realm f,
    userEmail :: ViewColumnar (NullableT Text) realm f
  }
  deriving (Generic)

$(defineHKDView ''UserSummaryF)

deriving instance Eq (UserSummaryInQuery ColumnName)
deriving instance Show (UserSummaryInQuery ColumnName)

spec :: Spec
spec = do
  describe "hkdViewDef" $ do
    it "generates correct view definition with snake_case columns" $ do
      let def = hkdViewDef @UserSummaryF "user_summary"

      viewName (def :: ViewDef UserSummaryInQuery) `shouldBe` "user_summary"

      let names = viewSelectedNames def
      names
        `shouldBe` ( UserSummary
                       { userId = MkColumnName "user_id",
                         userName = MkColumnName "user_name",
                         userEmail = MkColumnName "user_email"
                       } ::
                       UserSummaryInQuery ColumnName
                   )

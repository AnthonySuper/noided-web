{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Noided.Sql.Internal.TH.ViewSpec (spec) where

import Data.Functor.Const
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics
import Noided.Sql
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.HaskellT
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.Syntax
import Test.Hspec
import Prelude hiding (id)

-- A nested view.
data ContactF f = ContactF
  { email :: f (NullableT Text),
    phoneNumber :: f (NonNullT Text)
  }
  deriving (Generic)

$(deriveView ''ContactF)

-- Bare SQL-typed fields, a 'Col' field and a nested view, all in one view.
data UserSummaryF f = UserSummaryF
  { id :: f (NonNullT Int64),
    firstName :: f (NonNullT Text),
    middleName :: f (NullableT Text),
    postCount :: Col (RegularColumn Int64) f,
    contact :: ContactF f
  }
  deriving (Generic)

$(deriveView ''UserSummaryF)

userSummaryView :: ViewDef UserSummaryF
userSummaryView = viewSnakeCased "user_summary"

render :: (Query q) => q -> Text
render = renderSyntaxToTextNumberedBinds . renderQueryWriter . writeQuerySyntax

selectFirstNames :: SelectM (Element (SqlT NonNull Text) (SqlExpr NormalQuery))
selectFirstNames = do
  u <- addFrom_ (fromBase_ userSummaryView)
  addWhere_ (u.contact.phoneNumber ==. bindParam_ ("555" :: Text))
  pure (Element u.firstName)

decodedSummary :: UserSummaryF HaskellT
decodedSummary =
  UserSummaryF
    { id = HaskT 1,
      firstName = HaskT "bob",
      middleName = HaskT Nothing,
      postCount = HaskT 3,
      contact = ContactF {email = HaskT (Just "bob@example.com"), phoneNumber = HaskT "555"}
    }

plainSummary :: UserSummary
plainSummary =
  UserSummary
    { id = 1,
      firstName = "bob",
      middleName = Nothing,
      postCount = 3,
      contact = Contact {email = Just "bob@example.com", phoneNumber = "555"}
    }

unmatchedSummary :: UserSummaryNullF HaskellT
unmatchedSummary =
  UserSummaryNullF
    { id = HaskT Nothing,
      firstName = HaskT Nothing,
      middleName = HaskT Nothing,
      postCount = HaskT Nothing,
      contact = ContactNullF {email = HaskT Nothing, phoneNumber = HaskT Nothing}
    }

nameList :: (FFoldable t) => t ColumnName -> [Text]
nameList = ffoldMap (pure . getColumnName)

spec :: Spec
spec = do
  describe "deriveView" $ do
    it "has a column per field, nested views included" $
      flength (frepeat (Const "") :: UserSummaryF (Const Text)) `shouldBe` 6
    it "aliases columns by field name" $
      nameList (namedColumns :: UserSummaryF ColumnName)
        `shouldBe` ["id", "firstName", "middleName", "postCount", "email", "phoneNumber"]
    it "unwraps to the generated plain record" $
      unwrapSelectList decodedSummary `shouldBe` plainSummary
    it "unwraps an unmatched nullable copy to Nothing" $
      unwrapSelectList unmatchedSummary `shouldBe` Nothing

  describe "viewSnakeCased" $ do
    it "names the view" $
      userSummaryView.viewName `shouldBe` "user_summary"
    it "snake_cases column names" $
      nameList userSummaryView.viewSelectedNames
        `shouldBe` ["id", "first_name", "middle_name", "post_count", "email", "phone_number"]
    it "renders a select from the view" $
      render selectFirstNames
        `shouldBe` "SELECT \"user_summary\".\"first_name\" AS \"e\" FROM \"user_summary\" AS \"user_summary\" WHERE ((\"user_summary\".\"phone_number\") = ($1))"

  describe "viewWithNames" $ do
    it "uses the given names" $
      nameList
        ( viewWithNames
            "contacts"
            ContactF {email = "contact_email", phoneNumber = "contact_phone"}
        ).viewSelectedNames
        `shouldBe` ["contact_email", "contact_phone"]

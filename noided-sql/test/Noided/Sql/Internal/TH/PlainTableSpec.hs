{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Sql.Internal.TH.PlainTableSpec (spec) where

import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics
import Noided.Row
import Noided.Sql
import Noided.Sql.Internal.Class.Query
import Noided.Sql.Internal.Type.ColumnName
import Noided.Sql.Internal.Type.HaskellT
import Noided.Sql.Internal.Type.QueryWriter
import Noided.Sql.Internal.Type.Syntax
import Test.Hspec
import Prelude hiding (id)

-- A nested table.
data ProfileF f = ProfileF
  { bio :: Col (RegularColumn Text) f,
    websiteUrl :: Col (Column NoDefault Nullable Text) f
  }
  deriving (Generic)

$(defineTable ''ProfileF)

data UserF f = UserF
  { id :: Col (IdentityColumn Int64) f,
    name :: Col (RegularColumn Text) f,
    nick :: Col (Column MayBeDefault Nullable Text) f,
    profile :: ProfileF f
  }
  deriving (Generic)

$(defineTable ''UserF)

data PostF f = PostF
  { id :: Col (IdentityColumn Int64) f,
    userId :: Col (RegularColumn Int64) f,
    title :: Col (RegularColumn Text) f
  }
  deriving (Generic)

$(defineTable ''PostF)

usersTable :: TableDefinition (TableColumns UserF) UserF
usersTable = plainTableDef "users"

postsTable :: TableDefinition (TableColumns PostF) PostF
postsTable = plainTableDef "posts"

render :: (Query q) => q -> Text
render = renderSyntaxToTextNumberedBinds . renderQueryWriter . writeQuerySyntax

-- | Bare field access in a query.
selectNames :: SelectM (Element (SqlT NonNull Text) (SqlExpr NormalQuery))
selectNames = do
  u <- addFrom_ (fromBase_ usersTable)
  addWhere_ (u.name ==. bindParam ("bob" :: Text))
  pure (Element u.profile.bio)

userPosts :: SelectM ((UserF :-: PostF) (SqlExpr NormalQuery))
userPosts =
  addFrom_ $
    fromBase_ usersTable & innerJoin_ postsTable `on_` (\u p -> u.id ==. p.userId)

-- | Building a table row by hand in a query needs no annotation: 'Col'
-- reduces without knowing @f@, so @f@ is inferred from the field values.
-- (The Columnar version of this is ambiguous.)
handBuiltRow = do
  u <- addFrom_ (fromBase_ usersTable)
  p <- addFrom_ (fromBase_ postsTable)
  pure
    PostF
      { id = p.id,
        userId = u.id,
        title = u.name
      }

insertUser :: InsertQuery (Element (SqlT NonNull Int64) (SqlExpr NormalQuery))
insertUser =
  insertReturning
    usersTable
    ( singleValue_
        ( #name :==> mutateBound_ ("bob" :: Text)
            :::%? #bio :==> mutateBound_ ("hi" :: Text)
            :::%? EmptyWrappedRow
        )
    )
    (\u -> Element u.id)

spec :: Spec
spec = do
  describe "plain HKD tables" $ do
    it "renders a select with bare field access (nested fields are not prefixed)" $
      render selectNames
        `shouldBe` "SELECT \"users\".\"bio\" AS \"e\" FROM \"users\" AS \"users\" WHERE ((\"users\".\"name\") = ($1))"
    it "renders an inner join" $
      render userPosts
        `shouldBe` "SELECT \"users\".\"id\" AS \"id\", \"users\".\"name\" AS \"name\", \"users\".\"nick\" AS \"nick\", \"users\".\"bio\" AS \"bio\", \"users\".\"website_url\" AS \"websiteUrl\", \"posts\".\"id\" AS \"id_1\", \"posts\".\"user_id\" AS \"userId\", \"posts\".\"title\" AS \"title\" FROM \"users\" AS \"users\" INNER JOIN \"posts\" AS \"posts\" ON ((\"users\".\"id\") = (\"posts\".\"user_id\"))"
    it "renders a hand-built row without annotations" $
      render handBuiltRow
        `shouldBe` "SELECT \"posts\".\"id\" AS \"id\", \"users\".\"id\" AS \"userId\", \"users\".\"name\" AS \"title\" FROM \"users\" AS \"users\", \"posts\" AS \"posts\""
    it "renders an insert" $
      render insertUser
        `shouldBe` "INSERT INTO \"users\" AS to_insert (\"name\", \"bio\") VALUES ($1, $2) RETURNING to_insert.\"id\" AS \"e\""
    it "unwraps to the generated plain record" $ do
      let decoded :: UserF HaskellT
          decoded =
            UserF
              { id = HaskT 1,
                name = HaskT "bob",
                nick = HaskT Nothing,
                profile = ProfileF {bio = HaskT "hi", websiteUrl = HaskT (Just "x.com")}
              }
      unwrapSelectList decoded
        `shouldBe` User
          { id = 1,
            name = "bob",
            nick = Nothing,
            profile = Profile {bio = "hi", websiteUrl = Just "x.com"}
          }
    it "exposes flattened table columns with snake_cased names" $
      ffoldMap (\(MkColumnName n) -> [n]) (plainColumnNames @UserF)
        `shouldBe` ["id", "name", "nick", "bio", "website_url"]

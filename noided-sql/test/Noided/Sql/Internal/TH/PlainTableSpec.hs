-- Some bindings deliberately have no signature: they test inference.
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
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
import Data.Vector (Vector)
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

-- | Left join: the joined side is the generated @PostNullF@, with bare field access
-- whose types are already nullable. No annotation on the row itself.
userPostTitles = do
  (u :-: p) <-
    addFrom_ $
      fromBase_ usersTable & leftJoin_ postsTable `on_` (\u p -> u.id ==. p.userId)
  let title :: SqlExpr NormalQuery (NullableT Text)
      title = p.title
      titleOrPlaceholder :: SqlExpr NormalQuery (NonNullT Text)
      titleOrPlaceholder = coalesce_ p.title (bindParam ("?" :: Text))
      noPost :: SqlExpr NormalQuery (NonNullT Bool)
      noPost = isNull_ p.id
  addWhere_ (noPost ||. (title ==. bindParam ("hi" :: Text)))
  pure (Element u.name :*: Element titleOrPlaceholder)

-- | Nested table under a left join: every nested column is nullable too.
leftJoinedProfile :: SelectM ((PostF :-: UserNullF) (SqlExpr NormalQuery))
leftJoinedProfile = do
  (p :-: u) <-
    addFrom_ $
      fromBase_ postsTable & leftJoin_ usersTable `on_` (\p u -> u.id ==. p.userId)
  let bio :: SqlExpr NormalQuery (NullableT Text)
      bio = u.profile.bio
  addWhere_ (isNotNull_ bio)
  pure (p :-: u)

-- | Building a row by hand with no annotation: @f@ is inferred from the
-- field values, and there is no other type parameter to be ambiguous.
handBuiltRow = do
  u <- addFrom_ (fromBase_ usersTable)
  p <- addFrom_ (fromBase_ postsTable)
  pure PostF {id = p.id, userId = u.id, title = u.name}

-- | The hand-built row executes as a plain 'Post'.
handBuiltRowQuery :: TransactM e (Vector Post)
handBuiltRowQuery = queryVector handBuiltRow

-- | A hand-built row of the nullable copy, also with no annotation.
handBuiltNulledRow = do
  (_ :-: p) <-
    addFrom_ $
      fromBase_ usersTable & leftJoin_ postsTable `on_` (\u p -> u.id ==. p.userId)
  pure PostNullF {id = p.id, userId = p.userId, title = p.title}

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

-- Decoded rows, as the decoder would produce them.

decodedUser :: UserF HaskellT
decodedUser =
  UserF
    { id = HaskT 1,
      name = HaskT "bob",
      nick = HaskT Nothing,
      profile = ProfileF {bio = HaskT "hi", websiteUrl = HaskT (Just "x.com")}
    }

plainUser :: User
plainUser =
  User
    { id = 1,
      name = "bob",
      nick = Nothing,
      profile = Profile {bio = "hi", websiteUrl = Just "x.com"}
    }

-- | What a matched left-joined user decodes to: every column read nullably.
matchedNulledUser :: UserNullF HaskellT
matchedNulledUser =
  UserNullF
    { id = HaskT (Just 1),
      name = HaskT (Just "bob"),
      nick = HaskT Nothing,
      profile = ProfileNullF {bio = HaskT (Just "hi"), websiteUrl = HaskT (Just "x.com")}
    }

-- | What an unmatched left-joined user decodes to.
unmatchedNulledUser :: UserNullF HaskellT
unmatchedNulledUser =
  UserNullF
    { id = HaskT Nothing,
      name = HaskT Nothing,
      nick = HaskT Nothing,
      profile = ProfileNullF {bio = HaskT Nothing, websiteUrl = HaskT Nothing}
    }

-- | A matched user whose nested profile columns are given.
-- (Record update syntax is ambiguous here: 'UserF', 'UserNullF' and 'User'
-- share field names under DuplicateRecordFields.)
nulledUserWith :: HaskellT (NullableT Text) -> HaskellT (NullableT Text) -> UserNullF HaskellT
nulledUserWith bio url =
  UserNullF
    { id = HaskT (Just 1),
      name = HaskT (Just "bob"),
      nick = HaskT Nothing,
      profile = ProfileNullF {bio = bio, websiteUrl = url}
    }

spec :: Spec
spec = do
  describe "plain HKD tables" $ do
    it "renders a select with bare field access (nested fields are not prefixed)" $
      render selectNames
        `shouldBe` "SELECT \"users\".\"bio\" AS \"e\" FROM \"users\" AS \"users\" WHERE ((\"users\".\"name\") = ($1))"
    it "renders an inner join" $
      render userPosts
        `shouldBe` "SELECT \"users\".\"id\" AS \"id\", \"users\".\"name\" AS \"name\", \"users\".\"nick\" AS \"nick\", \"users\".\"bio\" AS \"bio\", \"users\".\"website_url\" AS \"websiteUrl\", \"posts\".\"id\" AS \"id_1\", \"posts\".\"user_id\" AS \"userId\", \"posts\".\"title\" AS \"title\" FROM \"users\" AS \"users\" INNER JOIN \"posts\" AS \"posts\" ON ((\"users\".\"id\") = (\"posts\".\"user_id\"))"
    it "renders a hand-built row with no annotation" $
      render handBuiltRow
        `shouldBe` "SELECT \"posts\".\"id\" AS \"id\", \"users\".\"id\" AS \"userId\", \"users\".\"name\" AS \"title\" FROM \"users\" AS \"users\", \"posts\" AS \"posts\""
    it "renders an insert" $
      render insertUser
        `shouldBe` "INSERT INTO \"users\" AS to_insert (\"name\", \"bio\") VALUES ($1, $2) RETURNING to_insert.\"id\" AS \"e\""
    it "unwraps to the generated plain record" $
      unwrapSelectList decodedUser `shouldBe` plainUser
    it "exposes flattened table columns with snake_cased names" $
      ffoldMap (\(MkColumnName n) -> [n]) (plainColumnNames @UserF)
        `shouldBe` ["id", "name", "nick", "bio", "website_url"]

  describe "left joins on plain HKD tables" $ do
    it "renders bare nullable field access, COALESCE and IS NULL" $
      render userPostTitles
        `shouldBe` "SELECT \"users\".\"name\" AS \"e\", COALESCE(\"posts\".\"title\",$1) AS \"e_1\" FROM \"users\" AS \"users\" LEFT JOIN \"posts\" AS \"posts\" ON ((\"users\".\"id\") = (\"posts\".\"user_id\")) WHERE (((\"posts\".\"id\") IS NULL) OR ((\"posts\".\"title\") = ($2)))"
    it "renders a nested table under a left join" $
      render leftJoinedProfile
        `shouldBe` "SELECT \"posts\".\"id\" AS \"id\", \"posts\".\"user_id\" AS \"userId\", \"posts\".\"title\" AS \"title\", \"users\".\"id\" AS \"id_1\", \"users\".\"name\" AS \"name\", \"users\".\"nick\" AS \"nick\", \"users\".\"bio\" AS \"bio\", \"users\".\"website_url\" AS \"websiteUrl\" FROM \"posts\" AS \"posts\" LEFT JOIN \"users\" AS \"users\" ON ((\"users\".\"id\") = (\"posts\".\"user_id\")) WHERE ((\"users\".\"bio\") IS NOT NULL)"
    it "decodes a matched left-joined row to Just" $
      unwrapSelectList matchedNulledUser `shouldBe` Just plainUser
    it "decodes an unmatched left-joined row to Nothing" $
      unwrapSelectList unmatchedNulledUser `shouldBe` Nothing
    it "keeps NULLs in declared-nullable columns of a matched row" $
      unwrapSelectList (nulledUserWith (HaskT (Just "hi")) (HaskT Nothing))
        `shouldBe` Just
          User {id = 1, name = "bob", nick = Nothing, profile = Profile {bio = "hi", websiteUrl = Nothing}}
    it "treats a NULL in any declared-NON NULL column (even nested) as a missing row" $
      unwrapSelectList (nulledUserWith (HaskT Nothing) (HaskT (Just "x.com")))
        `shouldBe` Nothing
    it "renders a hand-built nullable-copy row with no annotation" $
      render handBuiltNulledRow
        `shouldBe` "SELECT \"posts\".\"id\" AS \"id\", \"posts\".\"user_id\" AS \"userId\", \"posts\".\"title\" AS \"title\" FROM \"users\" AS \"users\" LEFT JOIN \"posts\" AS \"posts\" ON ((\"users\".\"id\") = (\"posts\".\"user_id\"))"
    it "unwraps a left join to (row, Maybe row)" $ do
      let joined :: (UserF :-: PostNullF) HaskellT
          joined = decodedUser :-: PostNullF {id = HaskT Nothing, userId = HaskT Nothing, title = HaskT Nothing}
          (u :--: mp) = unwrapSelectList joined
      u `shouldBe` plainUser
      mp `shouldBe` Nothing

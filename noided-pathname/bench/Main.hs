{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Control.DeepSeq (NFData (..))
import Criterion.Main
import Data.Text (Text)
import Noided.Pathname.Internal.PathTemplate qualified as PT
import Noided.Pathname.Internal.PieceTemplate qualified as Piece
import Noided.Pathname.Internal.Router

-- | A simple contained type for benchmarking
newtype Action captures = Action Text

instance NFData (Action captures) where
  rnf (Action n) = rnf n

-- Helper to build static routes for Router
buildStaticRouter :: [(Text, Text)] -> Router Action
buildStaticRouter = foldr (\(path, name) acc -> insertRouter (Piece.StaticPiece path PT.:/ PT.PathEnd) (Action name) acc) mempty

-- Helper to build dynamic routes for Router
buildDynamicRouter :: [Text] -> Router Action
buildDynamicRouter =
  foldr
    ( \resource acc ->
        insertRouter (Piece.StaticPiece resource PT.:/ Piece.capPiece @Int PT.:/ PT.PathEnd) (Action $ resource <> "_show") acc
    )
    mempty

-- Helper to build CRUD routes for Router
buildCrudRouter :: [Text] -> Router Action
buildCrudRouter =
  foldr
    ( \resource acc ->
        let base = Piece.StaticPiece resource
         in insertRouter (base PT.:/ PT.PathEnd) (Action $ resource <> "_index") $
              insertRouter (base PT.:/ Piece.StaticPiece "new" PT.:/ PT.PathEnd) (Action $ resource <> "_new") $
                insertRouter (base PT.:/ Piece.capPiece @Int PT.:/ PT.PathEnd) (Action $ resource <> "_show") $
                  insertRouter (base PT.:/ Piece.capPiece @Int PT.:/ Piece.StaticPiece "edit" PT.:/ PT.PathEnd) (Action $ resource <> "_edit") acc
    )
    mempty

-- Static paths benchmark data
staticPaths :: [(Text, Text)]
staticPaths =
  [ ("home", "home"),
    ("about", "about"),
    ("contact", "contact"),
    ("services", "services"),
    ("products", "products"),
    ("team", "team"),
    ("careers", "careers"),
    ("blog", "blog"),
    ("faq", "faq"),
    ("privacy", "privacy")
  ]

-- Dynamic paths benchmark data
dynamicResources :: [Text]
dynamicResources =
  [ "users",
    "posts",
    "comments",
    "articles",
    "products",
    "orders",
    "invoices",
    "customers",
    "vendors",
    "categories"
  ]

-- CRUD resources benchmark data
crudResources :: [Text]
crudResources =
  [ "users",
    "posts",
    "comments",
    "articles",
    "products",
    "orders",
    "reviews",
    "tags",
    "categories",
    "authors"
  ]

main :: IO ()
main = do
  let staticRouter = buildStaticRouter staticPaths
      dynamicRouter = buildDynamicRouter dynamicResources
      crudRouter = buildCrudRouter crudResources

  defaultMain
    [ bgroup
        "Static paths"
        [ bgroup
            "Router"
            [ bench "match /about" $ whnf (firstRouterMatch ["about"]) staticRouter,
              bench "match /contact" $ whnf (firstRouterMatch ["contact"]) staticRouter,
              bench "match /blog" $ whnf (firstRouterMatch ["blog"]) staticRouter,
              bench "miss /notfound" $ whnf (firstRouterMatch ["notfound"]) staticRouter
            ]
        ],
      bgroup
        "Dynamic paths"
        [ bgroup
            "Router"
            [ bench "match /users/42" $ whnf (firstRouterMatch ["users", "42"]) dynamicRouter,
              bench "match /posts/123" $ whnf (firstRouterMatch ["posts", "123"]) dynamicRouter,
              bench "match /articles/999" $ whnf (firstRouterMatch ["articles", "999"]) dynamicRouter,
              bench "miss /notfound/42" $ whnf (firstRouterMatch ["notfound", "42"]) dynamicRouter
            ]
        ],
      bgroup
        "CRUD app"
        [ bgroup
            "Router"
            [ bench "match /users (index)" $ whnf (firstRouterMatch ["users"]) crudRouter,
              bench "match /users/new" $ whnf (firstRouterMatch ["users", "new"]) crudRouter,
              bench "match /users/42 (show)" $ whnf (firstRouterMatch ["users", "42"]) crudRouter,
              bench "match /users/42/edit" $ whnf (firstRouterMatch ["users", "42", "edit"]) crudRouter,
              bench "match /posts (index)" $ whnf (firstRouterMatch ["posts"]) crudRouter,
              bench "match /posts/123/edit" $ whnf (firstRouterMatch ["posts", "123", "edit"]) crudRouter
            ]
        ]
    ]

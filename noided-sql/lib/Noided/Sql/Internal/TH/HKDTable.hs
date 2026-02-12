{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Noided.Sql.Internal.TH.HKDTable where

import Data.HKD
import Data.Text qualified as Text
import Language.Haskell.TH hiding (newName)
import Noided.Sql.Internal.Class.DecodeSelectList
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.Class.UnwrapSelectList
import Noided.Sql.Internal.Type.Columnar

defineHKDTable :: Name -> Q [Dec]
defineHKDTable name = do
  let nameText = Text.pack $ nameBase name
  nameStripped <-
    maybe (fail "name must end with an F") pure $
      Text.stripSuffix "F" nameText
  tableDefDecls <- defineTableDef name nameStripped
  inQueryDecls <- defineInQuery name nameStripped
  nullifiedInQueryDecls <- defineNullifiedInQuery name nameStripped
  unwraps <- defineUnwraps nameStripped
  return $
    [ TySynD
        (mkName $ Text.unpack nameStripped)
        []
        ( ConT name
            `AppT` PromotedT 'InHaskell
            `AppT` PromotedT '()
        ),
      TySynD
        (mkName $ Text.unpack (nameStripped <> "Nullified"))
        []
        ( ConT name
            `AppT` PromotedT 'NullifiedInHaskell
            `AppT` PromotedT '()
        )
    ]
      ++ tableDefDecls
      ++ inQueryDecls
      ++ nullifiedInQueryDecls
      ++ unwraps

defineTableDef :: Name -> Text.Text -> Q [Dec]
defineTableDef n t = do
  let newName = t <> "TableDef"
  defineHKDWrapper 'InTableDef n newName

defineInQuery :: Name -> Text.Text -> Q [Dec]
defineInQuery n t = do
  let newName = t <> "InQuery"
  hkds <- defineHKDWrapper 'InQuery n newName
  decoder <- defineDecoder (ConT $ mkName $ Text.unpack newName)
  return $
    hkds ++ decoder

defineNullifiedInQuery :: Name -> Text.Text -> Q [Dec]
defineNullifiedInQuery n t = do
  let newName = t <> "NullifiedInQuery"
  hkds <- defineHKDWrapper 'NullifiedInQuery n newName
  decoder <- defineDecoder (ConT $ mkName $ Text.unpack newName)
  return $ hkds ++ decoder

defineDecoder :: (Quote m) => Type -> m [Dec]
defineDecoder name' = [d|instance DecodeSelectList $name|]
  where
    name = pure name'

defineHKDWrapper :: Name -> Name -> Text.Text -> Q [Dec]
defineHKDWrapper toPromote originalName newName = do
  hkdDefaults <- defineHKDDefaults (ConT aliasName)
  nc <- defineNamedColumns (ConT aliasName)
  return $
    aliasDecl : (hkdDefaults ++ nc)
  where
    aliasDecl =
      TySynD aliasName [] $
        ConT originalName `AppT` PromotedT toPromote
    aliasName = mkName (Text.unpack $ newName)

defineNamedColumns :: (Quote m) => Type -> m [Dec]
defineNamedColumns name' =
  [d|instance NamedColumns $name|]
  where
    name = pure name'

defineUnwraps :: (Quote m) => Text.Text -> m [Dec]
defineUnwraps (unwrapped :: Text.Text) =
  [d|
    instance UnwrapSelectList $name where
      type
        SelectListUnwrapped $name =
          $normalAliasName
    |]
  where
    normalAliasName = pure $ ConT (mkName $ Text.unpack unwrapped)
    name = pure $ ConT (mkName $ Text.unpack $ unwrapped <> "InQuery")

defineHKDDefaults :: (Quote m) => Type -> m [Dec]
defineHKDDefaults name' =
  [d|
    instance FFunctor $name where
      ffmap = ffmapDefault

    instance FFoldable $name where
      ffoldMap = ffoldMapDefault

    instance FTraversable $name where
      ftraverse = gftraverse

    instance FRepeat $name where
      frepeat = gfrepeat

    instance FZip $name where
      fzipWith = gfzipWith
    |]
  where
    name = pure name'

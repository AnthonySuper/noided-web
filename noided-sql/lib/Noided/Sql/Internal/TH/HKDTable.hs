{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Noided.Sql.Internal.TH.HKDTable where

import Data.HKD
import Data.Text qualified as Text
import Language.Haskell.TH
import Noided.Sql.Internal.Class.NamedColumns
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

defineTableDef :: Name -> Text.Text -> Q [Dec]
defineTableDef = defineHKDWrapper 'InTableDef "TableDef"

defineInQuery :: Name -> Text.Text -> Q [Dec]
defineInQuery = defineHKDWrapper 'InQuery "InQuery"

defineNullifiedInQuery :: Name -> Text.Text -> Q [Dec]
defineNullifiedInQuery = defineHKDWrapper 'NullifiedInQuery "NullifiedInQuery"

defineHKDWrapper :: Name -> Text.Text -> Name -> Text.Text -> Q [Dec]
defineHKDWrapper toPromote suffix originalName strippedName = do
  hkdDefaults <- defineHKDDefaults (ConT aliasName)
  nc <- defineNamedColumns (ConT aliasName)
  return $
    aliasDecl : (hkdDefaults ++ nc)
  where
    aliasDecl =
      TySynD aliasName [] $
        ConT originalName `AppT` PromotedT toPromote
    aliasName = mkName (Text.unpack $ strippedName <> suffix)

defineNamedColumns :: (Quote m) => Type -> m [Dec]
defineNamedColumns name' =
  [d|instance NamedColumns $name|]
  where
    name = pure name'

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

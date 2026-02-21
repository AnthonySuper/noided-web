{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Noided.Sql.Internal.TH.HKDView
-- Description: Template Haskell helpers for HKD view types.
--
-- This module provides 'defineHKDView', which generates the necessary type
-- synonyms and instances for a data type defined using 'ViewColumnar'.
module Noided.Sql.Internal.TH.HKDView where

import Data.Text qualified as Text
import Language.Haskell.TH hiding (newName)
import Noided.Sql.Internal.TH.HKDTable (defineDecoder, defineHKDWrapper, defineUnwraps)
import Noided.Sql.Internal.Type.ViewColumnar

-- | Generate type synonyms and instances for an HKD view type.
--
-- Given a data type @MyViewF@ parameterised over 'ViewColumnUsage' and its
-- argument, this splice produces:
--
-- * @MyView@ — the plain Haskell record (@'ViewInHaskell'@)
-- * @MyViewNullified@ — the Haskell record with all fields wrapped in 'Maybe'
--   (@'ViewNullifiedInHaskell'@)
-- * @MyViewInQuery@ — the in-query HKD (@'ViewInQuery'@), with 'FFunctor',
--   'FTraversable', 'DecodeSelectList', etc.
-- * @MyViewNullifiedInQuery@ — the nullified in-query HKD
--   (@'ViewNullifiedInQuery'@)
-- * 'UnwrapSelectList' instances mapping the in-query types back to their
--   plain Haskell counterparts
--
-- === Example
--
-- > data UserViewF realm f = UserView
-- >   { userId   :: ViewColumnar (NonNullT Int64) realm f,
-- >     userName :: ViewColumnar (NonNullT Text)  realm f
-- >   }
-- >   deriving (Generic)
-- >
-- > $(defineHKDView ''UserViewF)
-- >
-- > userView :: ViewDef UserViewInQuery
-- > userView = hkdViewDef "user_view"
defineHKDView :: Name -> Q [Dec]
defineHKDView name = do
  let nameText = Text.pack $ nameBase name
  nameStripped <-
    maybe (fail "name must end with an F") pure $
      Text.stripSuffix "F" nameText
  inQueryDecls <- defineViewInQuery name nameStripped
  nullifiedInQueryDecls <- defineViewNullifiedInQuery name nameStripped
  unwraps <- defineUnwraps nameStripped
  return $
    [ TySynD
        (mkName $ Text.unpack nameStripped)
        []
        ( ConT name
            `AppT` PromotedT 'ViewInHaskell
            `AppT` PromotedT '()
        ),
      TySynD
        (mkName $ Text.unpack (nameStripped <> "Nullified"))
        []
        ( ConT name
            `AppT` PromotedT 'ViewNullifiedInHaskell
            `AppT` PromotedT '()
        )
    ]
      ++ inQueryDecls
      ++ nullifiedInQueryDecls
      ++ unwraps

defineViewInQuery :: Name -> Text.Text -> Q [Dec]
defineViewInQuery n t = do
  let newName = t <> "InQuery"
  hkds <- defineHKDWrapper 'ViewInQuery n newName
  decoder <- defineDecoder (ConT $ mkName $ Text.unpack newName)
  return $ hkds ++ decoder

defineViewNullifiedInQuery :: Name -> Text.Text -> Q [Dec]
defineViewNullifiedInQuery n t = do
  let newName = t <> "NullifiedInQuery"
  hkds <- defineHKDWrapper 'ViewNullifiedInQuery n newName
  decoder <- defineDecoder (ConT $ mkName $ Text.unpack newName)
  return $ hkds ++ decoder

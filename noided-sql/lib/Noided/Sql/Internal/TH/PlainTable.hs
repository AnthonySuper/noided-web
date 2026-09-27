{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Noided.Sql.Internal.TH.PlainTable
-- Description: Template Haskell for plain (realm-free) HKD tables.
--
-- Given
--
-- > data UserF f = UserF
-- >   { id :: Col (IdentityColumn Int64) f,
-- >     name :: Col (RegularColumn Text) f,
-- >     profile :: ProfileF f
-- >   }
-- >   deriving (Generic)
-- >
-- > $(defineTable ''UserF)
--
-- this generates:
--
-- * @data User = User { id :: Int64, name :: Text, profile :: Profile }@, a
--   real record (so you need @DuplicateRecordFields@ in the defining module);
-- * 'FFunctor', 'FFoldable', 'FTraversable', 'FRepeat', 'FZip',
--   'NamedColumns', 'DecodeSelectList' instances for @UserF@;
-- * @instance UnwrapSelectList UserF@ with @SelectListUnwrapped UserF = User@;
-- * @instance PlainTable UserF@, carrying the flattened column definitions
--   (defaults included) recovered from the /declared/ field types.
--
-- Nested HKD fields (@ProfileF f@) must themselves have been defined with
-- 'defineTable' earlier in the module (or imported).
module Noided.Sql.Internal.TH.PlainTable
  ( defineTable,
    defineTableDeriving,
  )
where

import Control.Monad (unless)
import Data.HKD
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Language.Haskell.TH
import Noided.Row
import Noided.Sql.Internal.Class.AsHaskellValue
import Noided.Sql.Internal.Class.DecodeSelectList
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.Class.UnwrapSelectList
import Noided.Sql.Internal.HKDTableDef (camelToSnake)
import Noided.Sql.Internal.Type.Col
import Noided.Sql.Internal.Type.ColumnName

-- | 'defineTableDeriving' with @Show@ and @Eq@ derived for the plain record.
defineTable :: Name -> Q [Dec]
defineTable = defineTableDeriving [''Show, ''Eq]

-- | Like 'defineTable', but choose which stock classes the generated plain
-- record derives. 'Generic' is always derived (it is needed for unwrapping).
defineTableDeriving :: [Name] -> Name -> Q [Dec]
defineTableDeriving derivs hkdName = do
  (conName, fields) <- reifyHKD hkdName
  plainName <- stripF hkdName
  unless (nameBase conName == nameBase hkdName) $
    fail $
      "defineTable: the constructor of "
        <> nameBase hkdName
        <> " must also be named "
        <> nameBase hkdName
        <> " (got "
        <> nameBase conName
        <> "), because the generated plain record "
        <> nameBase plainName
        <> " uses the name "
        <> nameBase plainName
        <> " for its constructor."
  classified <- traverse classifyField fields
  plainFields <- traverse plainField classified
  let plainDecl =
        DataD
          []
          plainName
          []
          Nothing
          [RecC plainName plainFields]
          [DerivClause Nothing (map ConT (''Generic : derivs))]
      hkdT = pure (ConT hkdName)
      plainT = pure (ConT plainName)
  hkdInstances <-
    [d|
      instance FFunctor $hkdT where
        ffmap = ffmapDefault

      instance FFoldable $hkdT where
        ffoldMap = ffoldMapDefault

      instance FTraversable $hkdT where
        ftraverse = gftraverse

      instance FRepeat $hkdT where
        frepeat = gfrepeat

      instance FZip $hkdT where
        fzipWith = gfzipWith

      instance NamedColumns $hkdT

      instance DecodeSelectList $hkdT

      instance UnwrapSelectList $hkdT where
        type SelectListUnwrapped $hkdT = $plainT
      |]
  tableInstance <- plainTableInstance hkdName classified
  pure $ plainDecl : hkdInstances ++ [tableInstance]

-- | A field of a plain HKD, after looking at its declared type.
data FieldKind
  = -- | @Col c f@, with the synonym-expanded column type @c@.
    ColumnField Name Type
  | -- | A nested plain HKD, @SubF f@.
    NestedField Name Name

reifyHKD :: Name -> Q (Name, [VarBangType])
reifyHKD n =
  reify n >>= \case
    TyConI (DataD _ _ [_] _ [RecC con fields] _) -> pure (con, fields)
    TyConI (NewtypeD _ _ [_] _ (RecC con fields) _) -> pure (con, fields)
    _ ->
      fail $
        "defineTable: "
          <> nameBase n
          <> " must be a single-constructor record with exactly one type parameter, like: data "
          <> nameBase n
          <> " f = "
          <> nameBase n
          <> " { ... }"

stripF :: Name -> Q Name
stripF n =
  case Text.stripSuffix "F" (Text.pack (nameBase n)) of
    Just s | not (Text.null s) -> pure (mkName (Text.unpack s))
    _ -> fail $ "defineTable: type name " <> nameBase n <> " must end with an F"

classifyField :: VarBangType -> Q FieldKind
classifyField (fname, _, ty) =
  case unApp (stripParens ty) of
    (ConT col, [c, VarT _]) | col == ''Col -> ColumnField fname <$> expandSyns c
    (ConT sub, [VarT _]) -> pure (NestedField fname sub)
    _ ->
      fail $
        "defineTable: field "
          <> nameBase fname
          <> " has type "
          <> pprint ty
          <> ", but fields must be either `Col (Column ...) f` or a nested table `SubF f`"

plainField :: FieldKind -> Q VarBangType
plainField k = do
  ty <- case k of
    ColumnField _ c -> columnHaskellType c
    NestedField _ sub -> ConT <$> stripF sub
  pure (mkName (nameBase (kindName k)), Bang NoSourceUnpackedness NoSourceStrictness, ty)

kindName :: FieldKind -> Name
kindName = \case
  ColumnField n _ -> n
  NestedField n _ -> n

-- | The Haskell type of a column, matching 'ColumnInHaskell'.
columnHaskellType :: Type -> Q Type
columnHaskellType c =
  case unApp c of
    (h, [_def, nullability, pgT])
      | isNamed "Column" h -> do
          ht <- resolveHaskellTypeOf pgT
          pure $
            if isNamed "Nullable" nullability
              then ConT ''Maybe `AppT` ht
              else ht
    _ -> fail $ "defineTable: could not read column type " <> pprint c <> " as `Column default nullability type`"

-- | Try to evaluate @HaskellTypeOf t@ at splice time, so the generated record
-- mentions the concrete type. Falls back to the type family application.
resolveHaskellTypeOf :: Type -> Q Type
resolveHaskellTypeOf t = do
  insts <- reifyInstances ''HaskellTypeOf [t]
  pure $ case insts of
    [TySynInstD (TySynEqn _ _ rhs)] | null (freeVars rhs) -> rhs
    _ -> ConT ''HaskellTypeOf `AppT` t

-- | The @PlainTable@ instance. Nested tables are flattened here (by reifying
-- them) rather than referenced through @TableColumns Sub@, so the instance is
-- a single literal list and the user's module doesn't need
-- @UndecidableInstances@.
plainTableInstance :: Name -> [FieldKind] -> Q Dec
plainTableInstance hkdName fields = do
  cols <- flattenColumns fields
  let colsTy =
        foldr
          ( \(n, c) acc ->
              PromotedConsT
                `AppT` (PromotedT '(:=>) `AppT` LitT (StrTyLit (nameBase n)) `AppT` c)
                `AppT` acc
          )
          PromotedNilT
          cols
  body <-
    foldr
      (\(n, _) acc -> [|MkColumnName (Text.pack $(litE (stringL (snake n)))) :::% $acc|])
      [|EmptyWrappedRow|]
      cols
  pure $
    InstanceD
      Nothing
      []
      (ConT ''PlainTable `AppT` ConT hkdName)
      [ TySynInstD (TySynEqn Nothing (ConT ''TableColumns `AppT` ConT hkdName) colsTy),
        ValD (VarP 'plainColumnNames) (NormalB body) []
      ]
  where
    snake = Text.unpack . camelToSnake . Text.pack . nameBase

flattenColumns :: [FieldKind] -> Q [(Name, Type)]
flattenColumns = fmap concat . traverse go
  where
    go = \case
      ColumnField n c -> pure [(n, c)]
      NestedField _ sub -> do
        (_, subFields) <- reifyHKD sub
        traverse classifyField subFields >>= flattenColumns

-- Type utilities

unApp :: Type -> (Type, [Type])
unApp = go []
  where
    go acc (AppT f x) = go (x : acc) f
    go acc (ParensT t) = go acc t
    go acc t = (t, acc)

stripParens :: Type -> Type
stripParens (ParensT t) = stripParens t
stripParens t = t

isNamed :: String -> Type -> Bool
isNamed s = \case
  ConT n -> nameBase n == s
  PromotedT n -> nameBase n == s
  _ -> False

-- | Expand type synonyms (e.g. 'IdentityColumn') so we can see the underlying
-- @Column d n t@.
expandSyns :: Type -> Q Type
expandSyns ty = do
  let (h, args) = unApp ty
  args' <- traverse expandSyns args
  case h of
    ConT n ->
      reify n >>= \case
        TyConI (TySynD _ bndrs rhs)
          | length args' >= length bndrs -> do
              let (now, rest) = splitAt (length bndrs) args'
                  sub = zip (map bndrName bndrs) now
              expandSyns (foldl AppT (substTy sub rhs) rest)
        _ -> pure (foldl AppT h args')
    _ -> pure (foldl AppT h args')

bndrName :: TyVarBndr a -> Name
bndrName = \case
  PlainTV n _ -> n
  KindedTV n _ _ -> n

substTy :: [(Name, Type)] -> Type -> Type
substTy sub = go
  where
    go = \case
      VarT n | Just t <- lookup n sub -> t
      AppT a b -> AppT (go a) (go b)
      ParensT t -> ParensT (go t)
      SigT t k -> SigT (go t) k
      t -> t

freeVars :: Type -> [Name]
freeVars = \case
  VarT n -> [n]
  AppT a b -> freeVars a ++ freeVars b
  ParensT t -> freeVars t
  SigT t _ -> freeVars t
  _ -> []

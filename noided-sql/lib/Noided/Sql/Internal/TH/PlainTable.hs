{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Noided.Sql.Internal.TH.PlainTable
-- Description: Template Haskell for plain (realm-free) HKD tables.
--
-- Given
--
-- > data UserF nt f = UserF
-- >   { id :: Col (IdentityColumn Int64) nt f,
-- >     name :: Col (RegularColumn Text) nt f,
-- >     profile :: ProfileF nt f
-- >   }
-- >   deriving (Generic)
-- >
-- > $(defineTable ''UserF)
--
-- this generates:
--
-- * @data User = User { id :: Int64, name :: Text, profile :: Profile }@, a
--   real record (so you need @DuplicateRecordFields@ in the defining module);
-- * @type UserQ = UserF 'NotNulled@;
-- * 'FFunctor', 'FFoldable', 'FTraversable', 'FRepeat', 'FZip' for @UserF nt@;
-- * 'NamedColumns', 'DecodeSelectList', 'Nullified' for both tags, with
--   @AsNullified (UserF tag) = UserF 'Nulled@;
-- * @SelectListUnwrapped (UserF 'NotNulled) = User@ and
--   @SelectListUnwrapped (UserF 'Nulled) = Maybe User@ (a custom type error
--   if the table has no NON NULL column to detect a missing row with);
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
import Noided.Sql.Internal.Class.DenullRow
import Noided.Sql.Internal.Class.Nullified
import Noided.Sql.Internal.Class.UnwrapSelectList
import GHC.TypeLits (ErrorMessage (Text), TypeError)
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
  cols <- flattenColumns classified
  let hkdT = pure (ConT hkdName)
      plainT = pure (ConT plainName)
      synName = mkName (nameBase plainName <> "Q")
      hasNonNull = any (isNonNullColumn . snd) cols
  -- Structural instances hold for every tag.
  hkdInstances <-
    [d|
      instance FFunctor ($hkdT nt) where
        ffmap = ffmapDefault

      instance FFoldable ($hkdT nt) where
        ffoldMap = ffoldMapDefault

      instance FTraversable ($hkdT nt) where
        ftraverse = gftraverse

      instance FRepeat ($hkdT nt) where
        frepeat = gfrepeat

      instance FZip ($hkdT nt) where
        fzipWith = gfzipWith

      instance DecodeSelectList ($hkdT NotNulled)

      instance DecodeSelectList ($hkdT Nulled)

      instance UnwrapSelectList ($hkdT NotNulled) where
        type SelectListUnwrapped ($hkdT NotNulled) = $plainT

      instance Nullified ($hkdT NotNulled) where
        type AsNullified ($hkdT NotNulled) = $hkdT Nulled

      instance Nullified ($hkdT Nulled) where
        type AsNullified ($hkdT Nulled) = $hkdT Nulled
        nullifyRow = id

      instance DenullRow $hkdT
      |]
  namedInstances <- namedColumnsInstances hkdT
  nulledUnwrap <-
    if hasNonNull
      then
        [d|
          instance UnwrapSelectList ($hkdT Nulled) where
            type SelectListUnwrapped ($hkdT Nulled) = Maybe $plainT
            unwrapSelectList = fmap unwrapSelectList . denullRow
          |]
      else do
        let msg =
              "Table "
                <> nameBase hkdName
                <> " has no NON NULL columns, so a missing outer-joined row cannot be told apart from a row of NULLs. Select its columns individually instead."
        [d|
          instance (TypeError (Text $(litT (strTyLit msg)))) => UnwrapSelectList ($hkdT Nulled) where
            type SelectListUnwrapped ($hkdT Nulled) = Maybe $plainT
            unwrapSelectList = error "unreachable"
          |]
  let synDecl = TySynD synName [] (ConT hkdName `AppT` PromotedT 'NotNulled)
  tableInstance <- plainTableInstance hkdName cols
  pure $ plainDecl : synDecl : hkdInstances ++ namedInstances ++ nulledUnwrap ++ [tableInstance]

-- | A field of a plain HKD, after looking at its declared type.
data FieldKind
  = -- | @Col c f@, with the synonym-expanded column type @c@.
    ColumnField Name Type
  | -- | A nested plain HKD, @SubF f@.
    NestedField Name Name

reifyHKD :: Name -> Q (Name, [VarBangType])
reifyHKD n =
  reify n >>= \case
    TyConI (DataD _ _ [_, _] _ [RecC con fields] _) -> pure (con, fields)
    TyConI (NewtypeD _ _ [_, _] _ (RecC con fields) _) -> pure (con, fields)
    _ ->
      fail $
        "defineTable: "
          <> nameBase n
          <> " must be a single-constructor record with exactly two type parameters, like: data "
          <> nameBase n
          <> " nt f = "
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
    (ConT col, [c, VarT _, VarT _]) | col == ''Col -> ColumnField fname <$> expandSyns c
    (ConT sub, [VarT _, VarT _]) -> pure (NestedField fname sub)
    _ ->
      fail $
        "defineTable: field "
          <> nameBase fname
          <> " has type "
          <> pprint ty
          <> ", but fields must be either `Col (Column ...) nt f` or a nested table `SubF nt f`"

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
plainTableInstance :: Name -> [(Name, Type)] -> Q Dec
plainTableInstance hkdName cols = do
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

isNonNullColumn :: Type -> Bool
isNonNullColumn c = case unApp c of
  (h, [_, nullability, _]) | isNamed "Column" h -> isNamed "NonNull" nullability
  _ -> False

-- | 'NamedColumns' for both tags, plus tag defaulting for hand-built rows.
--
-- A row built by hand, @PostF { id = p.id, ... }@, leaves its tag ambiguous:
-- @ApplyTag nt0 'NonNull ~ 'NonNull@ does not determine @nt0@. Every query
-- over the row needs @NamedColumns (PostF nt0)@, so we make that the place
-- where the tag defaults: the INCOHERENT @nt ~ 'NotNulled@ instance is chosen
-- while @nt0@ is still unknown, and the concrete @'Nulled@ instance (also
-- INCOHERENT, so it does not block that choice) wins once the tag is known.
--
-- Consequences: a hand-built row meant to be @'Nulled@ must say so
-- (@PostF \@Nulled ...@), and code polymorphic in the tag needs an explicit
-- @NamedColumns (PostF nt)@ constraint.
namedColumnsInstances :: Q Type -> Q [Dec]
namedColumnsInstances hkdT =
  [d|
    instance {-# INCOHERENT #-} (nt ~ NotNulled) => NamedColumns ($hkdT nt) where
      namedColumns = gnamedColumns

    instance {-# INCOHERENT #-} NamedColumns ($hkdT Nulled)
    |]

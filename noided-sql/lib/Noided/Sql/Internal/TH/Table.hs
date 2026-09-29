{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Noided.Sql.Internal.TH.Table
-- Description: Template Haskell for realm-free HKD tables.
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
-- > $(deriveTable ''UserF)
--
-- this generates:
--
-- * @data User = User { id :: Int64, name :: Text, profile :: Profile }@, a
--   real record (so you need @DuplicateRecordFields@ in the defining module);
-- * @data UserNullF f = UserNullF { id :: f (NullableT Int64), ..., profile :: ProfileNullF f }@,
--   the nullable copy used for the nullable side of outer joins;
-- * 'FFunctor', 'FFoldable', 'FTraversable', 'FRepeat', 'FZip',
--   'NamedColumns', 'DecodeSelectList' for both @UserF@ and @UserNullF@;
-- * 'Nullified' with @AsNullified UserF = UserNullF@ (and the copy mapping
--   to itself);
-- * @SelectListUnwrapped UserF = User@ and
--   @SelectListUnwrapped UserNullF = Maybe User@ (a custom type error if the
--   table has no NON NULL column to detect a missing row with);
-- * @instance Table UserF@, carrying the flattened column definitions
--   (defaults included) recovered from the /declared/ field types, and a
--   'toColumnRow' that flattens a row into them.
--
-- Nested HKD fields (@ProfileF f@) must themselves have been defined with
-- 'deriveTable' earlier in the module (or imported, along with their
-- generated @ProfileNullF@).
module Noided.Sql.Internal.TH.Table
  ( deriveTable,
    deriveTableWith,
  )
where

import Control.Monad (unless)
import Data.HKD
import Data.Text qualified as Text
import GHC.Generics (Generic)
import GHC.TypeLits (ErrorMessage (Text), TypeError)
import Language.Haskell.TH
import Noided.Row
import Noided.Sql.Internal.Class.AsHaskellValue
import Noided.Sql.Internal.Class.DecodeSelectList
import Noided.Sql.Internal.Class.DenullRow
import Noided.Sql.Internal.Class.NamedColumns
import Noided.Sql.Internal.Class.Nullified
import Noided.Sql.Internal.Class.UnwrapSelectList
import Noided.Sql.Internal.Type.Col
import Noided.Sql.Internal.Type.Nullability
import Noided.Sql.Internal.Type.SqlType

-- | 'deriveTableWith' with @Show@ and @Eq@ derived for the plain record.
deriveTable :: Name -> Q [Dec]
deriveTable = deriveTableWith [''Show, ''Eq]

-- | Like 'deriveTable', but choose which stock classes the generated plain
-- record derives. 'Generic' is always derived (it is needed for unwrapping).
deriveTableWith :: [Name] -> Name -> Q [Dec]
deriveTableWith derivs hkdName = do
  (conName, fields) <- reifyHKD hkdName
  plainName <- stripF hkdName
  let nullName = nullCopyName plainName
  unless (nameBase conName == nameBase hkdName) $
    fail $
      "deriveTable: the constructor of "
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
  fVar <- newName "f"
  nullFields <- traverse (nullField fVar) classified
  let plainDecl =
        DataD
          []
          plainName
          []
          Nothing
          [RecC plainName plainFields]
          [DerivClause Nothing (map ConT (''Generic : derivs))]
      nullDecl =
        DataD
          []
          nullName
          [PlainTV fVar dataBndrFlag]
          Nothing
          [RecC nullName nullFields]
          [DerivClause Nothing [ConT ''Generic]]
  cols <- flattenColumns classified
  let hkdT = pure (ConT hkdName)
      nullT = pure (ConT nullName)
      plainT = pure (ConT plainName)
      hasNonNull = any (isNonNullColumn . snd) cols
  hkdInstances <- concat <$> traverse structuralInstances [hkdT, nullT]
  tableInstances <-
    [d|
      instance UnwrapSelectList $hkdT where
        type SelectListUnwrapped $hkdT = $plainT

      instance Nullified $hkdT where
        type AsNullified $hkdT = $nullT

      instance Nullified $nullT where
        type AsNullified $nullT = $nullT
        nullifyRow = id

      instance DenullRow $nullT where
        type Denulled $nullT = $hkdT
      |]
  nullUnwrap <-
    if hasNonNull
      then
        [d|
          instance UnwrapSelectList $nullT where
            type SelectListUnwrapped $nullT = Maybe $plainT
            unwrapSelectList = fmap unwrapSelectList . denullRow
          |]
      else do
        let msg =
              "Table "
                <> nameBase hkdName
                <> " has no NON NULL columns, so a missing outer-joined row cannot be told apart from a row of NULLs. Select its columns individually instead."
        [d|
          instance (TypeError (Text $(litT (strTyLit msg)))) => UnwrapSelectList $nullT where
            type SelectListUnwrapped $nullT = Maybe $plainT
            unwrapSelectList = error "unreachable"
          |]
  tableInstance <- tableClassInstance hkdName conName classified cols
  pure $ plainDecl : nullDecl : hkdInstances ++ tableInstances ++ nullUnwrap ++ [tableInstance]

-- | Instances shared by a table and its nullable copy.
structuralInstances :: Q Type -> Q [Dec]
structuralInstances t =
  [d|
    instance FFunctor $t where
      ffmap = ffmapDefault

    instance FFoldable $t where
      ffoldMap = ffoldMapDefault

    instance FTraversable $t where
      ftraverse = gftraverse

    instance FRepeat $t where
      frepeat = gfrepeat

    instance FZip $t where
      fzipWith = gfzipWith

    instance NamedColumns $t

    instance DecodeSelectList $t
    |]

-- | @Post@ -> @PostNullF@.
nullCopyName :: Name -> Name
nullCopyName plain = mkName (nameBase plain <> "NullF")

-- | A field of the nullable copy: every column nullable, nested tables
-- pointing at their own nullable copy.
nullField :: Name -> FieldKind -> Q VarBangType
nullField f k = do
  ty <- case k of
    ColumnField _ c -> case unApp c of
      (_, [_def, _nullability, pgT]) ->
        pure $ VarT f `AppT` (PromotedT 'SqlT `AppT` PromotedT 'Nullable `AppT` pgT)
      _ -> fail $ "deriveTable: could not read column type " <> pprint c
    NestedField _ sub -> do
      subPlain <- stripF sub
      pure $ ConT (nullCopyName subPlain) `AppT` VarT f
  pure (mkName (nameBase (kindName k)), Bang NoSourceUnpackedness NoSourceStrictness, ty)

-- | A field of a table HKD, after looking at its declared type.
data FieldKind
  = -- | @Col c f@, with the synonym-expanded column type @c@.
    ColumnField Name Type
  | -- | A nested table HKD, @SubF f@.
    NestedField Name Name

reifyHKD :: Name -> Q (Name, [VarBangType])
reifyHKD n =
  reify n >>= \case
    TyConI (DataD _ _ [_] _ [RecC con fields] _) -> pure (con, fields)
    TyConI (NewtypeD _ _ [_] _ (RecC con fields) _) -> pure (con, fields)
    _ ->
      fail $
        "deriveTable: "
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
    _ -> fail $ "deriveTable: type name " <> nameBase n <> " must end with an F"

classifyField :: VarBangType -> Q FieldKind
classifyField (fname, _, ty) =
  case unApp (stripParens ty) of
    (ConT col, [c, VarT _]) | col == ''Col -> ColumnField fname <$> expandSyns c
    (ConT sub, [VarT _]) -> pure (NestedField fname sub)
    _ ->
      fail $
        "deriveTable: field "
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
    _ -> fail $ "deriveTable: could not read column type " <> pprint c <> " as `Column default nullability type`"

-- | Try to evaluate @HaskellTypeOf t@ at splice time, so the generated record
-- mentions the concrete type. Falls back to the type family application.
resolveHaskellTypeOf :: Type -> Q Type
resolveHaskellTypeOf t = do
  insts <- reifyInstances ''HaskellTypeOf [t]
  pure $ case insts of
    [TySynInstD (TySynEqn _ _ rhs)] | null (freeVars rhs) -> rhs
    _ -> ConT ''HaskellTypeOf `AppT` t

-- | The @Table@ instance. Nested tables are flattened here (by reifying
-- them) rather than referenced through @TableColumns Sub@, so the instance is
-- a single literal list and the user's module doesn't need
-- @UndecidableInstances@.
tableClassInstance :: Name -> Name -> [FieldKind] -> [(Name, Type)] -> Q Dec
tableClassInstance hkdName conName classified cols = do
  let colsTy =
        foldr
          ( \(n, c) acc ->
              PromotedConsT
                `AppT` (PromotedT '(:=>) `AppT` LitT (StrTyLit (nameBase n)) `AppT` c)
                `AppT` acc
          )
          PromotedNilT
          cols
  (pat, vars) <- flatPattern conName classified
  body <-
    foldr
      (\v acc -> [|QueryCol $(varE v) :::% $acc|])
      [|EmptyWrappedRow|]
      vars
  pure $
    InstanceD
      Nothing
      []
      (ConT ''Table `AppT` ConT hkdName)
      [ TySynInstD (TySynEqn Nothing (ConT ''TableColumns `AppT` ConT hkdName) colsTy),
        FunD 'toColumnRow [Clause [pat] (NormalB body) []]
      ]

-- | A pattern matching a row (and its nested tables) all the way down,
-- binding one variable per column, in declaration order.
flatPattern :: Name -> [FieldKind] -> Q (Pat, [Name])
flatPattern con kinds = do
  parts <- traverse go kinds
  pure (ConP con [] (map fst parts), concatMap snd parts)
  where
    go = \case
      ColumnField n _ -> do
        v <- newName (nameBase n)
        pure (VarP v, [v])
      NestedField _ sub -> do
        (subCon, subFields) <- reifyHKD sub
        traverse classifyField subFields >>= flatPattern subCon

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

-- | The flag on a data declaration's type variable binder. template-haskell
-- 2.21 (GHC 9.8) changed these from @()@ to 'BndrVis'.
#if MIN_VERSION_template_haskell(2, 21, 0)
dataBndrFlag :: BndrVis
dataBndrFlag = BndrReq
#else
dataBndrFlag :: ()
dataBndrFlag = ()
#endif

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

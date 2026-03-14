{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell helper for defining HKD forms.
--
-- See 'defineHKDForm' for usage.
module Noided.Form.HKD.TH
  ( defineHKDForm,
  )
where

import Data.HKD
import GHC.Generics (Generically)
import Language.Haskell.TH
import Noided.Form.HKD.Internal.Class (HKDForm)
import Noided.Form.HKD.Internal.Type.FormErrors (FormErrors)

-- | Derive all required instances for an HKD form type.
--
-- Given a type @FormF@ (which must derive 'GHC.Generics.Generic'), this generates:
--
-- * 'FFunctor', 'FFoldable', 'FTraversable', 'FRepeat', and 'FZip' instances
-- * 'Semigroup' and 'Monoid' instances for @FormF FormErrors@ via 'Generically'
-- * An 'HKDForm' instance
--
-- Example:
--
-- @
-- data MyFormF wrapper = MyForm { ... } deriving (Generic)
-- $(defineHKDForm \'\'MyFormF)
-- @
defineHKDForm :: Name -> Q [Dec]
defineHKDForm name = do
  let nameT = ConT name
  hkdDefs <- defineHKDFormDefaults nameT
  let formErrorsT = nameT `AppT` ConT ''FormErrors
      genericallyT = ConT ''Generically `AppT` formErrorsT
      viaSemigroup = StandaloneDerivD (Just (ViaStrategy genericallyT)) [] (ConT ''Semigroup `AppT` formErrorsT)
      viaMonoid = StandaloneDerivD (Just (ViaStrategy genericallyT)) [] (ConT ''Monoid `AppT` formErrorsT)
      hkdFormInst = InstanceD Nothing [] (ConT ''HKDForm `AppT` nameT) []
  return $ hkdDefs ++ [viaSemigroup, viaMonoid, hkdFormInst]

-- Internal helper that generates the HKD machinery instances using the
-- 'Quote'-polymorphic quasi-quoter. Kept separate from 'defineHKDForm' because
-- the Semigroup/Monoid/HKDForm declarations require direct AST construction.
defineHKDFormDefaults :: (Quote m) => Type -> m [Dec]
defineHKDFormDefaults name' =
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

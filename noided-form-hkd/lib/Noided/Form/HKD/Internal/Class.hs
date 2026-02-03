{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Form.HKD.Internal.Class where

import Data.HKD
import Data.Proxy
import GHC.Generics
import Noided.Form.HKD.Internal.Type.FormErrors
import Noided.Form.HKD.Internal.Type.FormLabel
import Noided.Form.HKD.Internal.Type.FormLens
import Noided.Form.HKD.Internal.Type.HKDFieldType
import Optics.Core hiding (to)

class
  ( FTraversable form,
    FRepeat form,
    Monoid (form FormErrors)
  ) =>
  HKDForm form
  where
  hkdFormLenses :: form (FormLens form)
  default hkdFormLenses ::
    (Generic (form (FormLens form)), GHKDFormLenses form (Rep (form (FormLens form)))) =>
    form (FormLens form)
  hkdFormLenses = ghkdFormLenses
  hkdFormLabels :: form FormLabel
  default hkdFormLabels ::
    (Generic (form FormLabel), GHKDFormLabels (Rep (form FormLabel))) =>
    form FormLabel
  hkdFormLabels = ghkdFormLabels
  hkdFormHasErrors :: form HasErrors

class GHKDFormLenses form rep where
  genericHKDFormLenses :: proxy form -> rep ()

instance
  (Is k A_Lens, forall wrapper. LabelOptic label k (form wrapper) (form wrapper) (wrapper (InputField t)) (wrapper (InputField t))) =>
  GHKDFormLenses form (S1 (MetaSel (Just name) su ss dl) (Rec0 (FormLens form (InputField t))))
  where
  genericHKDFormLenses _ = M1 $ K1 $ InputLens (castOptic @A_Lens @k $ labelOptic @label)

instance
  ( Is k A_Lens,
    forall wrapper. LabelOptic label k (form wrapper) (form wrapper) (wrapper (SubformField t)) (wrapper (SubformField t)),
    HKDForm t
  ) =>
  GHKDFormLenses form (S1 (MetaSel (Just name) su ss dl) (Rec0 (FormLens form (SubformField t))))
  where
  genericHKDFormLenses _ = M1 $ K1 $ SubformLens (castOptic @A_Lens @k $ labelOptic @label) hkdFormLenses

class ReflexLens wrapper where
  reflexLens :: FormLens (Element wrapper) wrapper

instance (ReflexLens (InputField i)) where
  reflexLens = InputLens elementLens

instance (HKDForm subform) => ReflexLens (SubformField subform) where
  reflexLens = SubformLens elementLens hkdFormLenses

instance (ReflexLens inner) => ReflexLens (ListField inner) where
  reflexLens = formLensReflex reflexLens

instance
  ( Is k A_Lens,
    forall wrapper. LabelOptic label k (form wrapper) (form wrapper) (wrapper (ListField t)) (wrapper (ListField t)),
    ReflexLens t
  ) =>
  GHKDFormLenses form (S1 (MetaSel (Just name) su ss dl) (Rec0 (FormLens form (ListField t))))
  where
  genericHKDFormLenses _ = M1 $ K1 $ ListLens (castOptic @A_Lens @k $ labelOptic @label) reflexLens

instance
  (GHKDFormLenses form lhs, GHKDFormLenses form rhs) =>
  GHKDFormLenses form (lhs :*: rhs)
  where
  genericHKDFormLenses proxy =
    genericHKDFormLenses proxy :*: genericHKDFormLenses proxy

instance (GHKDFormLenses form inner) => GHKDFormLenses form (C1 md inner) where
  genericHKDFormLenses = M1 . genericHKDFormLenses

instance (GHKDFormLenses form inner) => GHKDFormLenses form (D1 md inner) where
  genericHKDFormLenses = M1 . genericHKDFormLenses

ghkdFormLenses ::
  forall form.
  ( Generic (form (FormLens form)),
    GHKDFormLenses form (Rep (form (FormLens form)))
  ) =>
  form (FormLens form)
ghkdFormLenses = to (genericHKDFormLenses (Proxy @form))

class GHKDFormLabels rep where
  genericHKDFormLabels :: rep ()

ghkdFormLabels ::
  (Generic (form FormLabel), GHKDFormLabels (Rep (form FormLabel))) =>
  form FormLabel
ghkdFormLabels = to genericHKDFormLabels

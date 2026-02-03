{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Noided.Form.HKD.Internal.Class where

import Data.HKD
import Data.Proxy
import Data.Text (pack)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
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
  default hkdFormHasErrors ::
    (Generic (form HasErrors), GHKDFormHasErrors (Rep (form HasErrors))) =>
    form HasErrors
  hkdFormHasErrors = ghkdFormHasErrors

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

class HasFormLabelInner label where
  formLabelInner :: FormLabelInner label

instance HasFormLabelInner (InputField t) where
  formLabelInner = InputLabelInner

instance
  (HasFormLabelInner inner) =>
  HasFormLabelInner (ListField inner)
  where
  formLabelInner = ListLabelInner formLabelInner

instance
  (HKDForm subform) =>
  HasFormLabelInner (SubformField subform)
  where
  formLabelInner = SubformLabelInner hkdFormLabels

class GHKDFormLabels rep where
  genericHKDFormLabels :: rep ()

instance (GHKDFormLabels lhs, GHKDFormLabels rhs) => GHKDFormLabels (lhs :*: rhs) where
  genericHKDFormLabels = genericHKDFormLabels :*: genericHKDFormLabels

instance (GHKDFormLabels inner) => GHKDFormLabels (C1 md inner) where
  genericHKDFormLabels = M1 genericHKDFormLabels

instance (GHKDFormLabels inner) => GHKDFormLabels (D1 md inner) where
  genericHKDFormLabels = M1 genericHKDFormLabels

instance
  (KnownSymbol name, HasFormLabelInner t) =>
  GHKDFormLabels (S1 (MetaSel (Just name) su ss dl) (Rec0 (FormLabel t)))
  where
  genericHKDFormLabels = M1 $ K1 $ FormLabel (pack $ symbolVal (Proxy @name)) formLabelInner

ghkdFormLabels ::
  (Generic (form FormLabel), GHKDFormLabels (Rep (form FormLabel))) =>
  form FormLabel
ghkdFormLabels = to genericHKDFormLabels

class HasErrorEvidence field where
  hasErrors :: HasErrors field

instance HasErrorEvidence (InputField t) where
  hasErrors = InputHasErrors

instance (HKDForm subform) => HasErrorEvidence (SubformField subform) where
  hasErrors = SubformHasErrors hkdFormHasErrors

instance (HasErrorEvidence inner) => HasErrorEvidence (ListField inner) where
  hasErrors = ListHasErrors hasErrors

class GHKDFormHasErrors rep where
  genericHKDFormHasErrors :: rep ()

instance (GHKDFormHasErrors l, GHKDFormHasErrors r) => GHKDFormHasErrors (l :*: r) where
  genericHKDFormHasErrors =
    genericHKDFormHasErrors
      :*: genericHKDFormHasErrors

instance
  (GHKDFormHasErrors inner) =>
  GHKDFormHasErrors (M1 tag md inner)
  where
  genericHKDFormHasErrors = M1 genericHKDFormHasErrors

instance
  (HasErrorEvidence field) =>
  GHKDFormHasErrors (Rec0 (HasErrors field))
  where
  genericHKDFormHasErrors = K1 hasErrors

ghkdFormHasErrors ::
  (Generic (form HasErrors), GHKDFormHasErrors (Rep (form HasErrors))) =>
  form HasErrors
ghkdFormHasErrors = to genericHKDFormHasErrors

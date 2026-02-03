{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Noided.Form.HKD.Internal.Validate (validateHKDFormI) where

import Data.HKD
import Data.IntMap qualified as IM
import Data.Semigroup
import Data.Sequence qualified as Seq
import Data.These
import GHC.Generics
import Noided.Form.HKD.Internal.Type.FormErrors
import Noided.Form.HKD.Internal.Type.FormInput
import Noided.Form.HKD.Internal.Type.FormLens
import Noided.Form.HKD.Internal.Type.FormResult
import Noided.Form.HKD.Internal.Type.FormValidator
import Noided.Form.HKD.Internal.Type.HKDFieldType
import Noided.Validation
import Optics.Core

newtype AccumErrors m e a = AccumErrs {runAccumErrs :: m (Either e a)}
  deriving (Functor)

applyOverIM ::
  (Monad m) =>
  Int ->
  AccumErrors m (Endo (Element field FormErrors)) a ->
  AccumErrors m (IM.IntMap (Endo (FormErrors field))) a
applyOverIM i r = AccumErrs $ do
  res <- runAccumErrs r
  return $
    case res of
      Right r' -> Right r'
      Left (Endo mapErr) ->
        Left $
          IM.singleton i $
            Endo $
              \err -> case mapErr (Element err) of Element r' -> r'

infixr 6 **!

(**!) :: (FZip t) => t f -> t g -> t (f :*: g)
(**!) = fzipWith (:*:)

instance (Semigroup e, Monad m) => Applicative (AccumErrors m e) where
  pure = AccumErrs . return . Right
  f <*> a = AccumErrs $ do
    r1 <- runAccumErrs f
    fr2 <- runAccumErrs a
    return $
      case (r1, fr2) of
        (Left e, Left e') -> Left $ e <> e'
        (Right _, Left e') -> Left e'
        (Left e, Right _) -> Left e
        (Right f', Right a') -> Right $ f' a'

validateHKDFormI ::
  ( Monad m,
    FTraversable subform,
    FRepeat subform,
    Monoid (subform FormErrors)
  ) =>
  subform (FormLens subform) ->
  subform HasErrors ->
  FormValidator m (SubformField subform) ->
  subform FormInput ->
  m (Either (FormErrors (SubformField subform)) (subform FormResult))
validateHKDFormI lenses hasErrors validator input = do
  res <-
    runAccumErrs $
      validateHKDForm'
        ( SubformInput input
            :*: validator
            :*: SubformHasErrors hasErrors
            :*: SubformLens elementLens lenses
        )
  return $
    case res of
      Right (SubformResult r) -> Right r
      Left e -> Left $ case appEndo e (Element mempty) of
        Element errs -> errs

validateHKDForm' ::
  forall outerForm field m.
  (Monad m) =>
  (FormInput :*: FormValidator m :*: HasErrors :*: FormLens outerForm) field ->
  AccumErrors m (Endo (outerForm FormErrors)) (FormResult field)
validateHKDForm' = \case
  (fi :*: BaseValidator validateBase validateInner :*: he :*: fl) -> AccumErrs $ do
    res <- runValidatorTThese (validateBase fi)
    let mapBase baseErrs = Endo $ over (baseLens fl) (<> onlyBaseErrors baseErrs)
    case res of
      This fatal ->
        return $
          Left $
            mapBase fatal
      That newInput ->
        runAccumErrs $ validateHKDForm' (newInput :*: validateInner :*: he :*: fl)
      These nonFatal newInput -> do
        r <- runAccumErrs $ validateHKDForm' (newInput :*: validateInner :*: he :*: fl)
        return $
          case r of
            Left bad ->
              Left $ mapBase nonFatal <> bad
            Right _ ->
              Left $ mapBase nonFatal
  (InputInput input :*: InputValidator ve :*: _ :*: InputLens se) -> AccumErrs $ do
    res <- runValidatorT (ve input)
    return $
      case res of
        Right r -> Right (InputResult r)
        Left e -> Left $ Endo (se %~ (<> InputErrors e))
  (SubformInput se :*: SubformValidator ve :*: SubformHasErrors ie :*: SubformLens bl fieldLenses) -> AccumErrs $ do
    ran <- runAccumErrs $ ftraverse validateHKDForm' (se **! ve **! ie **! fieldLenses)
    return $
      case ran of
        Right r -> Right (SubformResult r)
        Left (Endo e) -> Left $ Endo $ over (bl % #innerErrors) e
  (ListInput li :*: ListValidator vl :*: ListHasErrors innerErrs :*: ListLens bl ll) -> AccumErrs $ do
    res <-
      runAccumErrs $
        Seq.traverseWithIndex
          ( \idx inp ->
              applyOverIM idx $ validateHKDForm' (inp :*: vl :*: innerErrs :*: ll)
          )
          li
    return $
      case res of
        Right r -> Right (ListResult r)
        Left e -> Left $ Endo $ over (bl % listInnerErrors) (<> (fmap (`appEndo` emptyErrorsFromEvidence innerErrs) e))

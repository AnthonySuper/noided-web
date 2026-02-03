{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Noided.Form.HKD.Internal.Render (renderHKDField') where

import Data.Foldable
import Data.Functor
import Data.Functor.Const
import Data.HKD
import Data.IntMap qualified as IM
import Data.Sequence qualified as Seq
import GHC.Generics
import Noided.Form
import Noided.Form.HKD.Internal.Type.FormErrors
import Noided.Form.HKD.Internal.Type.FormInput
import Noided.Form.HKD.Internal.Type.FormLabel
import Noided.Form.HKD.Internal.Type.FormRenderer

infixr 6 **!

(**!) :: (FZip t) => t f -> t g -> t (f :*: g)
(**!) = fzipWith (:*:)

renderHKDTuple ::
  (Monad m) =>
  FormCanonicalKey ->
  (FormInput :*: FormErrors :*: FormLabel :*: FormRenderer m) field ->
  m (Const () field)
renderHKDTuple fk (fi :*: fe :*: FormLabel l i :*: fr) =
  renderHKDField' (newContext :*: fr :*: i) $> Const ()
  where
    newContext =
      RenderContext
        fi
        fe
        (fk `appendCanonicalPiece` CanonicalObjectPiece l)

renderHKDList ::
  (Monad m) =>
  FormCanonicalKey ->
  IM.IntMap (FormErrors field) ->
  FormLabelInner field ->
  FormRenderer m field ->
  Int ->
  FormInput field ->
  m ()
renderHKDList fk errs fl render i fi = renderHKDField' (newCtx :*: render :*: fl)
  where
    newCtx =
      RenderContext
        { key = fk `appendCanonicalPiece` CanonicalArrayPiece i,
          input = fi,
          errors = fold (IM.lookup i errs)
        }

renderHKDField' ::
  forall field m.
  (Monad m) =>
  (RenderingContext :*: FormRenderer m :*: FormLabelInner) field ->
  m ()
renderHKDField' = \case
  (ctx :*: AroundRendering cb re :*: fli) ->
    cb ctx (renderHKDField' $ ctx :*: re :*: fli)
  (ctx :*: InputRenderer rf :*: InputLabelInner) ->
    rf ctx
  (RenderContext (SubformInput inp) sfe k :*: SubformRenderer sf :*: SubformLabelInner sfi) ->
    let (SubformErrors _ fieldErrs) = sfe
     in ftraverse (renderHKDTuple k) (inp **! fieldErrs **! sfi **! sf) $> ()
  (RenderContext (ListInput inpt) (ListErrors _ errs) k :*: ListRenderer renderInner :*: ListLabelInner li) ->
    Seq.traverseWithIndex (renderHKDList k errs li renderInner) inpt $> ()

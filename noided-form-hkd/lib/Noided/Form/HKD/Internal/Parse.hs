module Noided.Form.HKD.Internal.Parse where

import Data.HKD
import Data.Maybe
import Noided.Form
import Noided.Form.HKD.Internal.Type.FormInput
import Noided.Form.HKD.Internal.Type.FormLabel
import Optics.Core

parseForm :: FormLabel field -> FormSubmission MultipartFormData -> FormInput field
parseForm field sub = case field of
  InputLabel t ->
    InputInput $
      maybe
        NotPresent
        FromForm
        (sub ^? _SubmissionObject % at t % non' _SubmissionEmpty % _SubmissionValue)
  SubformLabel t sf ->
    let ns = fromMaybe (SubmissionObject mempty) (sub ^? _SubmissionObject % at t % _Just)
     in SubformInput (ffmap (`parseForm` ns) sf)
  ListLabel t sf ->
    let ns =
          case sub ^? _SubmissionObject % at t % non' _SubmissionEmpty of
            Just (SubmissionArray a) -> a
            Just v -> pure v
            Nothing -> mempty
     in ListInput $ fmap (parseForm sf) ns

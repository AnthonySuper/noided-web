module Noided.Form.HKD.Internal.Parse (parseForm') where

import Data.HKD
import Data.Maybe
import Noided.Form
import Noided.Form.HKD.Internal.Type.FormInput
import Noided.Form.HKD.Internal.Type.FormLabel
import Optics.Core

parseForm' :: FormLabel field -> FormSubmission MultipartFormData -> FormInput field
parseForm' (FormLabel field inner) sub =
  parseFormInner inner (fromMaybe SubmissionEmpty $ sub ^? _SubmissionObject % at field % _Just)

parseFormInner :: FormLabelInner field -> FormSubmission MultipartFormData -> FormInput field
parseFormInner fl sub =
  case fl of
    InputLabelInner ->
      InputInput $
        maybe NotPresent FromForm (sub ^? _SubmissionValue)
    ListLabelInner ll ->
      ListInput $
        fmap (parseFormInner ll) $
          case sub of
            SubmissionArray a -> a
            o -> pure o
    SubformLabelInner sl ->
      SubformInput $
        ffmap (`parseForm'` sub) sl

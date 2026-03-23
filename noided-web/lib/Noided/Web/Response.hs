module Noided.Web.Response
  ( -- * Page Responses
    PageResponse (..),
    respondPage,
    respondPage200,
    respondHKDForm,
    respondHKDForm',
    liftPageResponseRendering,
    addPageResponseLayout,
    PageResponseType (..),
    pageResponseToResponse,

    -- * Redirect Types
    RedirectType (..),

    -- * Response Bodies
    ResponseBody (..),

    -- * Raw Responses
    Response (..),
  )
where

import Lucid
import Network.HTTP.Types.Status
import Noided.Form.HKD
import Noided.Validation
import Noided.Web.Error
import Noided.Web.Internal.Type.Response

internalErrorToStatus :: SomeValidationError -> Maybe Status
internalErrorToStatus err
  | Just (Unauthorized _) <- fromSomeValidationError err = Just unauthorized401
  | Just (Forbidden _) <- fromSomeValidationError err = Just forbidden403
  | Just (NotFound _) <- fromSomeValidationError err = Just notFound404
  | Just (Conflict _) <- fromSomeValidationError err = Just conflict409
  | otherwise = Nothing

respondHKDForm :: (HtmlT renderM () -> HtmlT renderM ()) -> (FormErrors field -> HtmlT renderM ()) -> FormErrors field -> PageResponse renderM
respondHKDForm = respondHKDForm' internalErrorToStatus

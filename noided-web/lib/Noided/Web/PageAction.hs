module Noided.Web.PageAction
  ( -- * Page Routes
    PageRoutes,
    pagesAddLayout,
    pagesAroundAction,
    pagesAroundResponse,
    pagesProvideTranslations,
    pagesProvideReaderEnv,
    pagesHandleErrorM,
    pagesHandleError,

    -- * Routing
    actGet,
    actPost,
    actHead,
    actPut,
    actDelete,
    actTrace,
    actConnect,
    actOptions,

    -- * Page Actions
    PageAction,
    SomePageAction,
    aroundPageAction,
    hoistPageActionMonad,
  )
where

import Noided.Web.Internal.Type.PageAction

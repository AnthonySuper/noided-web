# Action Integration Patterns

Use these patterns to integrate forms into `optimize-beer` Actions.

## GET (Displaying the form)

```haskell
-- In OptBeer.Action.MyAction
newMyFormAction :: (FetchMessages renderM, FetchHtmlFormatters renderM, Monad m) => RouteParams '[] -> m (PageResponse renderM)
newMyFormAction (RPNil :: RouteParams '[]) =
  return $
    respondPage200 $
      wrapForm $
        renderFormT myFormRenderer hkdFormEmpty mempty
```

## POST (Handling submission)

```haskell
-- In OptBeer.Action.MyAction
handleMyFormAction ::
  ( Error BadRequest :> es,
    Error SessionError :> es,
    GetRequestBody :> es,
    RunTransaction :> es,
    FetchMessages renderM,
    FetchHtmlFormatters renderM
  ) =>
  RouteParams '[] ->
  Eff es (PageResponse renderM)
handleMyFormAction (RPNil :: RouteParams '[]) = do
  body <- hkdFormBody
  result <- runTransactionEither $ validateForm myFormValidator body
  case result of
    Left errs ->
      return $
        RespondFormErrors
          wrapForm
          (renderFormT myFormRenderer body errs)
    Right validated -> do
      -- 'validated' is of type 'MyFormF FormResult'
      -- Access fields with 'validated.field1.val' (if using OverloadedRecordDot)
      -- or 'view #field1 validated'
      runTransaction $ performBusinessLogic validated
      return $ RespondRedirect RedirectFound "/"
```

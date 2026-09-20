# Noided-Pathname

`noided-pathname` is a *fast*, *type-safe*, *dependent* URL-router for Haskell, supporting capturing parameters in a GADT like this:

```haskell
data RouteParams (t :: [Type]) where
  RPNil :: RouteParams '[]
  (:-$) :: t -> RouteParams rest -> RouteParams (t ': rest)
```

This type is equivalent to an `hlist`, a list where each item can be a *different type*.
It's parameterized by the *list of types* that match up with the values of the list.

Just having a type like this is pretty boring.
What's more interesting is that we can perform *routing* over URLs.
A router can route to any type of kind `[Type] -> Type`, for example:

```haskell
newtype ControllerAction capturedParams = Act { runAct :: RouteParams capturedParams -> IO HttpResponse }
```

When a URL is routed, each *capture* is parsed from peices of the URL, and stored in a `RouteParams` along with the matched route.
A matched route is defined as:

```haskell
data RouteMatch contained where
  RouteMatched ::
    RouteParams captures ->
    contained captures ->
    RouteMatch contained
```

So, given the action above, we could do this:

```haskell

httpApplication :: Router ControllerAction
httpApplication = mempty -- we'll show you how to fill this in later

runMatch :: [Text] -> Maybe (IO HttpResponse)
runMatch urlPieces =
  case firstRouterMatch urlPieces httpApplication of
    Nothing -> Nothing -- we can handle this with a 404 later
    Just (RouteMatched params action) ->
      -- Here, we know that `params` has type `RouteParams urlParamTypes`,
      -- and `action` has type `ControllerAction urlParamTypes`.
      -- We don't know what `urlParamTypes` *is*, but that doesn't matter: we know it's the same in both,
      -- so we can do this:
      Just $ action.runAct params
```

This enables dependently-typed routing: the *type* of your actions can depend on the *parameters* of your URLs, enhancing type-safety.
And it's pretty damn fast, too.

## Installation

`noided-pathname` is not on hackage yet.
Once it is, you can...

Add `noided-pathname` to your cabal file's build-depends:

```cabal
build-depends:
    base >= 4.18 && < 5,
    noided-pathname,
    text,
    http-api-data
```

## Getting Started

After adding `noided-pathname` to your cabal file, you can import `Noided.Pathname`.
This module contains everything you need to:

- Define path templates
- Define path routers
- Route paths

For example, here's how we might build a router for controller actions:

```haskell
newtype UserId = MkUserId { getUserId :: UUID }
  deriving (Show, Read, Eq, Ord)
  -- | Need these classes implemented to use this type as a URL parameter
  deriving (FromHttpApiData, ToHttpApiData) via UUID

httpApplication :: Router ControllerAction
httpApplication =
  singletonRouter (staticPiece "users" :/ PathEnd) indexUserAction
  -- or, use overloaded strings:
    <> singletonRouter ("users" :/ capPiece @UserId :/ PathEnd) showUserAction
    <> singletonRouter ("users" :/ capPiece @UserId :/ "edit" :/ PathEnd) editUserAction
```

To route actions, you can use `firstRouterMatch`:

```haskell
runMatch :: [Text] -> Maybe (IO HttpResponse)
runMatch urlPieces = do
  -- RouteMatch includes the parsed parameters, as well as the matched action
  RouteMatched params action <- firstRouterMatch urlPieces httpApplication
  return $ runAct action params
```

### Using path templates

The path templates used with `singletonRouter` are their own data type, with some useful features.
Instead of defining them inline when routing, it's useful to define them in separate variables:

```haskell

userIndexPath :: PathTemplate '[]
userIndexPath = "users" :/ PathEnd

userShowPath :: PathTemplate '[UserId]
userShowPath =
  userIndexPath
    `appendPathTemplate`
      (capPiece @UserId :/ PathEnd)

userEditPath :: PathTemplate '[UserId]
userEditPath = userShowPath `appendPathTemplate` ("edit" :/ PathEnd)
```

This will not only prevent typos, but let us use these path templates in other ways.
For example, we can generate a *url* from the template:

```haskell
userEditLink :: UserId -> Text
userEditLink uid = usePathTemplateParams userEditPath (uid :-$ RPNil)
```

Neat, huh?

### Percent-encoding, and turning a URL back into pieces

`usePathTemplate` and `usePathTemplateParams` percent-encode every piece they emit, captures and static pieces alike.
A capture containing a `/`, `?`, `#`, a space, or a non-ASCII character is escaped, so it stays a single URL piece instead of silently generating a link to a different route:

```haskell
>>> usePathTemplateParams ("users" :/ capPiece @Text :/ PathEnd) ("a/b" :-$ RPNil)
"/users/a%2Fb/"
```

Going the other way, `splitPathPieces` turns a URL path into the `[Text]` that `firstRouterMatch` and `matchPathTemplate` expect, percent-decoding each piece:

```haskell
>>> splitPathPieces "/users/a%2Fb/"
["users","a/b",""]
```

The leading empty piece from the leading `/` is dropped, so the result is ready to route.
A *trailing* `/` is kept as a trailing empty piece, which is harmless: both matchers treat a single trailing empty piece as the end of the path, so a URL matches the same templates with or without a trailing slash.
`splitPathPieces` is the inverse of `usePathTemplateParams`, which always emits a trailing slash:

```haskell
>>> let template = "users" :/ capPiece @Int :/ PathEnd
>>> matchPathTemplate (splitPathPieces (usePathTemplateParams template (42 :-$ RPNil))) template
Right (42 :-$ RPNil)
```

If you are serving over WAI, you do not need this: `Network.Wai.pathInfo` already gives you decoded pieces in the same shape.

### Testing and Debugging Routes

When you have a routing error, you often want to figure out what went wrong.
The function `testUrlResult` will test a given path element list against everything in a router, and give you detailed information about match failures:

```haskell
testUrlResult :: [Text] -> Router action -> [(Some PathTemplate, TemplateMatchResult)]

-- Example usage:
let results = testUrlResult (splitPathPieces "/users/invalid-id") httpApplication
-- Results will show which routes were tried and why they failed
```

This is particularly useful during development and debugging to understand why a particular URL isn't matching your expected route.
It matches URLs exactly the way the real router does, so a route it reports as matching really does route, and one it rejects really does 404.

## Features

- **Type-safe parameter capture**: URL parameters are tracked at the type level, ensuring you can only access parameters that exist in the route
- **Fast routing**: Efficient tree-based router structure for quick lookups
- **Bidirectional routing**: Generate URLs from templates and parameters
- **Composable routers**: Combine routers using `<>` (Semigroup)
- **OverloadedStrings support**: Write routes naturally with string literals
- **Flexible matching**: Support for static paths, captured parameters, and combinations

## Performance

The router is designed for performance with:
- Tree-based structure for efficient route lookup
- Static route segments are matched first (before dynamic captures)
- Minimal allocations during routing
- Benchmarks available in the `bench/` directory

To run benchmarks:
```bash
cabal bench noided-pathname
```

## Contributing

Issues and pull requests are welcome.
This library is experimental and extracted from a Haskell web framework I am trying to write.

## License

AGPL-3.0-or-later



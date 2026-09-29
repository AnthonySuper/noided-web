# Revision history for noided-pathname

## Unreleased

* **Breaking**: `:-$` is now `infixr 5` rather than `infixl 3`, matching `:/`.
  Multi-capture params can now be written without parentheses, as
  `5 :-$ 42 :-$ RPNil`; under the old fixity that did not typecheck at all.
  Any call site that worked around this by parenthesizing by hand can drop the
  parentheses.
* URL generation (`usePathTemplate`, `usePathTemplateParams`) now percent-encodes
  every piece it emits, captures and static pieces alike. Previously a capture
  containing a `/`, `?`, `#`, or a space generated a URL for a different route.
* Added `splitPathPieces`, which splits and percent-decodes a URL path into the
  pieces `firstRouterMatch` and `matchPathTemplate` expect, and `encodePathPiece`,
  which escapes a single piece. This adds a dependency on `http-types`.
* `matchPathTemplate` now treats an empty piece as the end of the path only in
  final position, the way the router always has. `["users", "5", "", "junk"]` no
  longer reports a match for a route that would 404 in production.
* Fixed `testEquality` on `PathTemplate` being asymmetric: a static piece on the
  left against a capture on the right returned `Nothing` where the mirrored case
  returned `Just Refl`.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

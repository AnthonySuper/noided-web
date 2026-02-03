# Developer Notes for `noided-web`

## Working with `noided-form-hkd`

### Defining HKD Forms

When defining a new HKD form, you cannot simply derive `HKDForm`. You must satisfy its superclass constraints (`FTraversable`, `FRepeat`, `Monoid (form FormErrors)`).

**Template:**

```haskell
data MyForm f = MyForm
  { field1 :: f (InputField Text)
  , field2 :: f (SubformField OtherForm)
  }
  deriving (Generic)

-- Standard boilerplate for HKD instances
instance FFunctor MyForm where ffmap = ffmapDefault
instance FFoldable MyForm where ffoldMap = ffoldMapDefault
instance FTraversable MyForm where ftraverse = gftraverse
instance FZip MyForm where fzipWith = gfzipWith
instance FRepeat MyForm where frepeat = gfrepeat

-- Monoid instance for errors is required
deriving via (Generically (MyForm FormErrors)) instance Semigroup (MyForm FormErrors)
deriving via (Generically (MyForm FormErrors)) instance Monoid (MyForm FormErrors)

-- Finally, derive HKDForm
instance HKDForm MyForm
```

### Accessing Errors

*   `FormErrors` wraps `ValidationErrors` and potentially inner errors (for subforms/lists).
*   Use `.innerErrors` to access the underlying errors.
    *   For `InputField`, `.innerErrors` returns `ValidationErrors`.
    *   For `SubformField`, `.innerErrors` returns the subform's errors (HKD structure).
    *   For `ListField`, `.innerErrors` returns `IntMap` of item errors.

### Validation Logic

*   `validateForm` returns `Either (FormErrors ...) (subform FormResult)`.
*   Partial success (warnings/non-fatal errors) results in `Left` containing the accumulated errors.
*   `check` (non-fatal) vs `require` (fatal) works as expected, but any error prevents `Right`.

### Common Pitfalls

1.  **`ValidationError` Derivation**: Deriving `ValidationError` via `Generic` requires `noided-translate` instances (`ToMessageParam`) for all fields. If these are missing or you want to avoid dependencies, implement `ValidationError` manually.
2.  **Extensions**: `DerivingVia`, `UndecidableInstances`, `DuplicateRecordFields`, and `OverloadedRecordDot` are frequently needed.

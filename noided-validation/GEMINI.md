# `noided-validation` Knowledge Base

## Overview
`noided-validation` is a Haskell validation library built on a custom Monad Transformer stack. It is designed to handle both fatal (abort immediately) and non-fatal (collect and continue) validation errors, with a strong focus on type-safe, translatable error messages.

## Core Architecture

### `ValidatorT`
The core type is `ValidatorT m a`, effectively a wrapper around `ExceptT ValidationErrors (WriterT ValidationErrors m) a`.
*   **Non-Fatal Errors**: Use `check` or `failNonfatal`. These accumulate errors in the `WriterT` layer and allow execution to proceed.
*   **Fatal Errors**: Use `require` or `failFatal`. These abort execution via `ExceptT`.
*   **Alternative**: The `<|>` instance allows branching. If the left branch fails, the right is tried. If *both* fail, errors are merged.

### `ValidationError` Typeclass
Any data type can be a validation error if it implements the `ValidationError` class.
*   **`SomeValidationError`**: An existential wrapper allowing heterogeneous sets of errors.
*   **`validationErrorKey`**: Defaults to the type name. Used for serialization/translation keys.
*   **`validationErrorTranslateParams`**: Uses Generics to automatically derive parameters for translation.

## Project Structure & Conventions

### Adding New Validations
1.  **Define Error Type**: Create a new module in `Noided.Validation.Internal.ValidationError.<Category>`.
    *   Derive `Generic` and `ValidationError`.
    *   Example: `data TooSmall = ... deriving (Generic, ValidationError)`
2.  **Define Validator**: Create a new module in `Noided.Validation.Internal.Validate.<Category>`.
    *   Import the error module.
    *   Implement functions returning `ValidatorT m ()`.
3.  **Expose Modules**: Add both new modules to `exposed-modules` in `.cabal`.

### Testing
*   Tests are located in `test/`.
*   They mirror the library structure (e.g., `test/Noided/Validation/Internal/Validate/TextSpec.hs`).
*   Run tests with `cabal test`.

## Useful Snippets

### Basic Validator Implementation
```haskell
import Noided.Validation.Internal.Validator

validateSomething :: (Monad m) => Int -> ValidatorT m ()
validateSomething val =
  if val > 10
    then return ()
    else failNonfatal $ MyError val
```

### Combined Check
```haskell
validateComplex :: (Monad m) => Data -> ValidatorT m ()
validateComplex d = do
  require (isValidBasic d) BasicError -- Fatal: stop if this fails
  check (isNice d) NiceError          -- Non-fatal: record if fails, keep going
  check (isClean d) CleanError
```

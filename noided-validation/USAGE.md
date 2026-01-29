# Using the noided-validation Public API

The `noided-validation` library now provides a clean public API for validation. You can import all functionality through the unified `Noided.Validation` module.

## Quick Start

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

import Noided.Validation
import Data.Text (Text)
import GHC.Generics (Generic)

-- Define your custom error types
data UserError
  = EmailTooShort
  | EmailTooLong
  | AgeTooYoung
  | AgeTooOld
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

-- Write validation functions
validateEmail :: Text -> Validator ()
validateEmail email = do
  lengthAtLeast 5 email
  lengthAtMost 100 email
  contains "@" email

validateAge :: Int -> Validator ()
validateAge age = do
  require (age >= 0) AgeTooYoung  -- Fatal error
  check (age < 150) AgeTooOld     -- Non-fatal error

-- Combine validations
validateUser :: Text -> Int -> Validator ()
validateUser email age = do
  validateEmail email
  validateAge age

-- Run validation
main :: IO ()
main = do
  case runValidator (validateUser "user@example.com" 25) of
    Right _ -> putStrLn "Valid user!"
    Left errs -> print errs
```

## Available Modules

### Core Modules

- `Noided.Validation` - Unified module with all functionality
- `Noided.Validation.Validator` - Validator monad and core functions
- `Noided.Validation.ValidationError` - Error type classes
- `Noided.Validation.ValidationErrors` - Error collection types

### Validation Modules

- `Noided.Validation.Validate.Blank` - Check for empty/blank values
- `Noided.Validation.Validate.Combinators` - Validation combinators
- `Noided.Validation.Validate.Number` - Number validations (odd/even)
- `Noided.Validation.Validate.Selection` - Membership validations
- `Noided.Validation.Validate.Size` - Size and range validations
- `Noided.Validation.Validate.Text` - Text pattern validations

## Key Features

### Fatal vs Non-fatal Errors

- Use `require` for fatal errors that stop validation immediately
- Use `check` for non-fatal errors that allow validation to continue

```haskell
validateInput :: Int -> Validator ()
validateInput n = do
  require (n >= 0) NegativeNumber  -- Stops if negative
  check (n < 100) TooLarge         -- Continues even if too large
  check (n `mod` 2 == 0) NotEven   -- Continues collecting errors
```

### Validation Combinators

```haskell
-- Optional validation
optional validateEmail (Just email)

-- Conditional validation
validateIf (isAdmin user) validateAdminFields
```

### Size Validations

```haskell
-- For collections
lengthAtLeast 2 items
lengthAtMost 10 items
lengthBetween 2 10 items

-- For values
valueAtLeast 0 age
valueAtMost 150 age
valueBetween 0 150 age
```

### Text Validations

```haskell
startsWith "https://" url
endsWith ".com" domain
contains "@" email
notContains "<script>" userInput
```

## Migration from Internal Modules

If you were previously importing from `Noided.Validation.Internal.*` modules, you can now import from the public API:

**Before:**
```haskell
import Noided.Validation.Internal.Validator
import Noided.Validation.Internal.ValidationErrors
import Noided.Validation.Internal.Validate.Text
```

**After:**
```haskell
import Noided.Validation
```

The internal modules are still available for backwards compatibility, but the public API is recommended for new code.

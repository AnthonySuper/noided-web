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
  = InvalidEmail Text
  | AgeTooYoung
  | AgeTooOld
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ValidationError)

-- Write validation functions
validateEmail :: Text -> Validator ()
validateEmail email = do
  check (length email >= 5) (InvalidEmail "Email too short")
  check (length email <= 100) (InvalidEmail "Email too long")
  check ("@" `elem` email) (InvalidEmail "Email must contain @")

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

### Built-in Validators

The library provides built-in validators that generate their own error types:

```haskell
-- These validators generate appropriate error messages automatically
lengthAtLeast 5 someList   -- Generates TooSmall error if needed
startsWith "https://" url  -- Generates DoesNotStartWith error if needed
```

These are convenient for common validation patterns, but you can also use `check` with custom errors for full control.

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

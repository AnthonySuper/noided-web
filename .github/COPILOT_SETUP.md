# GitHub Copilot Agent Setup Instructions

This document provides setup instructions for GitHub Copilot agents working on the noided-web repository.

## Overview

This repository contains multiple Haskell packages managed by Cabal:
- `noided-form`: Form handling utilities
- `noided-pathname`: Pathname utilities
- `noided-row`: Row type utilities
- `noided-translate`: Translation utilities
- `noided-validation`: Validation utilities

## Quick Setup

Run the automated setup script:

```bash
# Make the script executable (if not already)
chmod +x ./.github/copilot-setup.sh

# Run the setup script
./.github/copilot-setup.sh
```

Alternatively, you can run it without making it executable:

```bash
bash ./.github/copilot-setup.sh
```

This script will:
1. Install ghcup (Haskell toolchain installer)
2. Install GHC (Glasgow Haskell Compiler) version 9.8.2
3. Install Cabal version 3.10
4. Update cabal package list
5. Configure the project with tests enabled
6. Install all dependencies
7. Build all packages
8. Run all tests

## Manual Setup

If you prefer to set up manually or need to customize the setup:

### Prerequisites

- ghcup (Haskell toolchain installer)
- GHC 9.6.7 or 9.8.2 (as tested in CI)
- Cabal 3.10

### Installation Steps

1. **Install Haskell toolchain:**
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   ```

2. **Install specific GHC and Cabal versions:**
   ```bash
   ghcup install ghc 9.8.2
   ghcup install cabal 3.10
   ghcup set ghc 9.8.2
   ghcup set cabal 3.10
   ```

3. **Update cabal:**
   ```bash
   cabal update
   ```

4. **Configure the project:**
   ```bash
   cabal configure --enable-tests
   ```

5. **Install dependencies:**
   ```bash
   cabal build --only-dependencies --enable-tests --enable-benchmarks all
   ```

6. **Build all packages:**
   ```bash
   cabal build --enable-tests --enable-benchmarks all
   ```

7. **Run tests:**
   ```bash
   cabal test all
   ```

## Common Development Commands

### Building

```bash
# Build all packages
cabal build all

# Build a specific package
cabal build noided-form
cabal build noided-pathname
cabal build noided-row
cabal build noided-translate
cabal build noided-validation
```

### Testing

```bash
# Run all tests
cabal test all

# Run tests for a specific package
cabal test noided-form
cabal test noided-pathname
cabal test noided-row
cabal test noided-translate
cabal test noided-validation
```

### Cleaning

```bash
# Clean build artifacts
cabal clean
```

### REPL

```bash
# Start REPL for a specific package
cabal repl noided-form
```

## CI Configuration

This repository uses GitHub Actions for continuous integration. The CI workflow:
- Tests with GHC versions 9.6.7 and 9.8.2
- Uses Cabal version 3.10
- Builds all packages with tests and benchmarks enabled
- Runs the complete test suite

The CI configuration can be found in `.github/workflows/ci.yml`.

## Troubleshooting

### Dependencies not found

If you encounter dependency issues, try:
```bash
cabal update
cabal clean
cabal build --only-dependencies all
```

### GHC version mismatch

Ensure you're using the correct GHC version:
```bash
ghc --version  # Should show 9.6.7 or 9.8.2
ghcup set ghc 9.8.2
```

### Cache issues

If you encounter cache-related issues:
```bash
rm -rf dist-newstyle
cabal clean
cabal build all
```

## Project Structure

```
noided-web/
├── .github/
│   ├── workflows/
│   │   └── ci.yml              # CI configuration
│   ├── copilot-setup.sh        # Automated setup script
│   └── COPILOT_SETUP.md        # This file
├── cabal.project               # Multi-package project configuration
├── noided-form/                # Form handling package
├── noided-pathname/            # Pathname utilities package
├── noided-row/                 # Row type utilities package
├── noided-translate/           # Translation utilities package
└── noided-validation/          # Validation utilities package
```

Each package contains:
- `*.cabal` - Package configuration
- `lib/` - Source code
- `test/` - Test suite

## Environment Variables

You can customize the setup script behavior with environment variables:

- `GHC_VERSION`: GHC version to install (default: 9.8.2)
- `CABAL_VERSION`: Cabal version to install (default: 3.10)

Example:
```bash
GHC_VERSION=9.6.7 ./.github/copilot-setup.sh
```

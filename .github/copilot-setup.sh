#!/bin/bash
# GitHub Copilot Agent Setup Script
# This script sets up the development environment for the noided-web repository
# Based on the CI workflow defined in .github/workflows/ci.yml

set -e  # Exit on error
set -u  # Exit on undefined variable
set -o pipefail  # Exit on pipe failure

echo "============================================"
echo "GitHub Copilot Agent Setup for noided-web"
echo "============================================"
echo ""

# Configuration
GHC_VERSION="${GHC_VERSION:-9.8.2}"
CABAL_VERSION="${CABAL_VERSION:-3.10}"

echo "Configuration:"
echo "  GHC Version: $GHC_VERSION"
echo "  Cabal Version: $CABAL_VERSION"
echo ""

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Check if ghcup is installed
if ! command_exists ghcup; then
    echo "Installing ghcup (Haskell toolchain installer)..."
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
    
    # Source ghcup environment
    export PATH="$HOME/.ghcup/bin:$PATH"
    
    echo "ghcup installed successfully"
    echo ""
else
    echo "ghcup is already installed"
    echo ""
fi

# Ensure ghcup is in PATH
export PATH="$HOME/.ghcup/bin:$PATH"

# Install GHC
echo "Installing GHC $GHC_VERSION..."
if ghcup list | grep "ghc.*$GHC_VERSION" | grep -q "installed"; then
    echo "GHC $GHC_VERSION is already installed"
else
    ghcup install ghc "$GHC_VERSION"
    echo "GHC $GHC_VERSION installed successfully"
fi
ghcup set ghc "$GHC_VERSION"
echo ""

# Install Cabal
echo "Installing Cabal $CABAL_VERSION..."
if ghcup list | grep "cabal.*$CABAL_VERSION" | grep -q "installed"; then
    echo "Cabal $CABAL_VERSION is already installed"
else
    ghcup install cabal "$CABAL_VERSION"
    echo "Cabal $CABAL_VERSION installed successfully"
fi
ghcup set cabal "$CABAL_VERSION"
echo ""

# Update cabal package list
echo "Updating cabal package list..."
cabal update
echo ""

# Configure the project
echo "Configuring the project..."
cabal configure --enable-tests
echo "Project configured successfully"
echo ""

# Install dependencies
echo "Installing dependencies..."
cabal build --only-dependencies --enable-tests --enable-benchmarks all
echo "Dependencies installed successfully"
echo ""

# Build all packages
echo "Building all packages..."
cabal build --enable-tests --enable-benchmarks all
echo "Build completed successfully"
echo ""

# Run tests
echo "Running tests..."
cabal test all
echo "Tests completed successfully"
echo ""

echo "============================================"
echo "Setup completed successfully!"
echo "============================================"
echo ""
echo "You can now work on the noided-web repository."
echo ""
echo "Common commands:"
echo "  cabal build all          - Build all packages"
echo "  cabal test all           - Run all tests"
echo "  cabal build <package>    - Build a specific package"
echo "  cabal test <package>     - Test a specific package"
echo ""
echo "Available packages:"
echo "  - noided-form"
echo "  - noided-pathname"
echo "  - noided-row"
echo "  - noided-translate"
echo "  - noided-validation"
echo ""

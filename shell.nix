# Development environment configuration for a Nix-based project
# This file creates an isolated environment with all neccessary tools and dependencies.

{ pkgs ? import <nixpkgs> { } }:

let
  # We'll use a specific version of GHC for consistency
  ghcVersion = "ghc984";

  # Define out haskell package set with the GHC version we want
  haskellPackages = pkgs.haskell.packages.${ghcVersion};

  # List of Haskell dependencies our project needs
  # These will be available in the development environment
  haskellDeps = with haskellPackages; [
    # Web framework and server
    servant
    servant-server
    warp
    wai

    # Data types and serizalization
    aeson
    text
    bytestring
  ];

  # Development tools that help with haskell development
  devTools = with haskellPackages; [
    cabal-install # Build tool for Haskell projects 
    haskell-language-server # IDE support
    fourmolu # Haskell code formatter
    hlint # Haskell linter
    ghcid # Auto-recompiling Haskell code
    hoogle # Documentation search
  ];

in
pkgs.mkShell {
  # Name of our shell environment
  name = "todo-postgresql";

  # Packages to include in the environment
  buildInputs = with pkgs; [
    # GHC and Haskell packages
    haskellPackages.ghc

    # Nix development tools
    nixpkgs-fmt # Nix formatter
    nil # Nix LSP 
  ]
  ++ haskellDeps
  ++ devTools
  ++ [
    # System tools
    zlib # Required by some Haskell packages

    # Development utilities
    git # Version control system
    gnumake # Build automation tool
    entr # File watcher for auto-reloading
  ];

  # Environment variables
  shellHook = ''
    echo "Welcome to TODO API development environment"
    echo ""
    
    if command -v ghc &> /dev/null; then
      ghc_version=$(ghc --version | grep -oE '[0-9]+\.[0-9]+\.[0-9]+')
      echo "✓ GHC $ghc_version"
    else
      echo "✗ GHC not found!"
    fi

    if command -v cabal &> /dev/null; then
      echo "✓ Cabal available"
    else
      echo "✗ Cabal not found!"
    fi    

    if command -v haskell-language-server &> /dev/null; then
      echo "✓ Haskell language server available"
    else
      echo "✗ Haskell language server not found!"
    fi

    if command -v nil &> /dev/null; then
      echo "✓ nil (Nix LSP) available"
    else
      echo "✗ nil not found!"
    fi
    
    if command -v nixpkgs-fmt &> /dev/null; then
      echo "✓ nixpkgs-fmt available"
    else
      echo "✗ nixpkgs-fmt not found!"
    fi
  '';

  # Prevent garbage collection of dependencies while in shell
  # This ensures our development environment stays stable
  NIX_ENFORCE_PURITY = 0;
}

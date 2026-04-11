{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    # Haskell toolchain
    stack
    haskell.compiler.ghc96   # pin a specific GHC version
    haskell-language-server

    # JDK build dependencies
    jdk17
    autoconf
    automake
    unzip
    zip
    libX11
    libXext
    libXrender
    libXtst
    libXt
    cups
    freetype
    alsa-lib
  ];

  # Make Stack use Nix-provided GHC instead of downloading its own
  shellHook = ''
    export STACK_IN_NIX_SHELL=1
    echo "Benchmarking environment loaded"
  '';
}

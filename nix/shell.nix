{ pkgs ? import <nixpkgs> {} }:
# Used for building JDKs and benchmarking in general
pkgs.mkShell {
  buildInputs = with pkgs; 
    [ 
      jdk25
      autoconf.out
      alsa-lib
      alsa-lib.dev
      libX11
      freetype
      cups
      fontconfig
      freetype.dev
      freetype.out
      libx11
      libxi
      libxext
      libxrender
      libxtst
      libxt
      libxrandr
      libx11.dev
      libxext.dev
      libxrender.dev
      libxt.dev
      libxtst
      libxrandr.dev
      libxi.dev
      ];
  nativeBuildInputs = with pkgs; [ pkg-config xorgproto ];

  shellHook = ''
    export NIX_SHELL_NAME="jdk-build"
    export SOURCE_DATE_EPOCH=1672531200
  '';
}

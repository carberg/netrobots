let
  pkgs = import <nixpkgs> {};
in
  { stdenv ? pkgs.stdenv }:
            
  stdenv.mkDerivation {
    name = "netrobotsBoardViewer";
    version = "0.1.0.0";
    src = ./.;
    buildInputs = [
      stdenv
      pkgs.elmPackages.elm
    ];
  }

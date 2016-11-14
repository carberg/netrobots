let
  pkgs = import <nixpkgs> {};
in
  { stdenv ? pkgs.stdenv, python27 ? pkgs.python27, pyp ? pkgs.python27Packages }:
            
  stdenv.mkDerivation {
    name = "netrobots";
    version = "0.1.0.0";
    src = ./.;
    buildInputs = [
      stdenv
      python27
      pyp.tornado
      pyp.six
      pyp.urllib3
      pyp.certifi
      pkgs.elmPackages.elm
    ];
  }

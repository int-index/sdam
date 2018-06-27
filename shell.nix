# { stdenv, pkgs, haskell }:
with import <nixpkgs> { };

stdenv.mkDerivation rec {
  name = "sdam";
  buildInputs = [
    haskell.compiler.ghc842
    pkgs.cabal-install
  ];
}

{ pkgs ? import <nixpkgs> {},
  hc ? "ghc865"
}:

pkgs.stdenv.mkDerivation rec {
  name = "sdam";
  buildInputs = [
    pkgs.haskell.compiler.${hc}
    pkgs.cabal-install
  ];
}

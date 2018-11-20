{ pkgs ? import <nixpkgs> {},
  hc ? "ghc844"
}:

pkgs.stdenv.mkDerivation rec {
  name = "sdam";
  buildInputs = [
    pkgs.haskell.compiler.${hc}
    pkgs.cabal-install
  ];
}

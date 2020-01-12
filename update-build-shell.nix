with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "update-build";
  buildInputs = [ (haskellPackages.ghcWithPackages (p: with p; [hpack cabal2nix])) ];
  buildCommand = "";
}

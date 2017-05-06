with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "update-build";
  buildInputs = [ cabal2nix haskellPackages.hpack ];
  buildCommand = "";
}

{ mkDerivation, base, exceptions, hpack, hspec, lens, monad-control
, mtl, profunctors, QuickCheck, stdenv, template-haskell, text
, transformers, transformers-base, unliftio-core, vinyl
}:
mkDerivation {
  pname = "composite-base";
  version = "0.7.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base exceptions lens monad-control mtl profunctors template-haskell
    text transformers transformers-base unliftio-core vinyl
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base exceptions hspec lens monad-control mtl profunctors QuickCheck
    template-haskell text transformers transformers-base unliftio-core
    vinyl
  ];
  prePatch = "hpack";
  homepage = "https://github.com/ConferOpenSource/composite#readme";
  description = "Shared utilities for composite-* packages";
  license = stdenv.lib.licenses.bsd3;
}

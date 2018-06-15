{ mkDerivation, base, exceptions, hspec, lens, monad-control, mtl
, profunctors, QuickCheck, stdenv, template-haskell, text
, transformers, transformers-base, unliftio-core, vinyl
}:
mkDerivation {
  pname = "composite-base";
  version = "0.5.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base exceptions lens monad-control mtl profunctors template-haskell
    text transformers transformers-base unliftio-core vinyl
  ];
  testHaskellDepends = [
    base exceptions hspec lens monad-control mtl profunctors QuickCheck
    template-haskell text transformers transformers-base unliftio-core
    vinyl
  ];
  homepage = "https://github.com/ConferHealth/composite#readme";
  description = "Shared utilities for composite-* packages";
  license = stdenv.lib.licenses.bsd3;
}

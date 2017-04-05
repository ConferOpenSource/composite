{ mkDerivation, base, basic-prelude, Frames, hspec, lens
, monad-control, mtl, QuickCheck, stdenv, template-haskell, text
, transformers, transformers-base, vinyl
}:
mkDerivation {
  pname = "composite-base";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base basic-prelude Frames lens monad-control mtl template-haskell
    text transformers transformers-base vinyl
  ];
  testHaskellDepends = [
    base basic-prelude Frames hspec lens monad-control mtl QuickCheck
    template-haskell text transformers transformers-base vinyl
  ];
  homepage = "https://github.com/ConferHealth/composite#readme";
  description = "Shared utilities for composite-* packages";
  license = stdenv.lib.licenses.bsd3;
}

{ mkDerivation, base, basic-prelude, Frames, lens, stdenv
, template-haskell, text, vinyl
}:
mkDerivation {
  pname = "composite-base";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base basic-prelude Frames lens template-haskell text vinyl
  ];
  homepage = "https://github.com/ConferHealth/composite#readme";
  description = "Shared utilities for composite-* packages";
  license = stdenv.lib.licenses.bsd3;
}

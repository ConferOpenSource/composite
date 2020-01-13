{ mkDerivation, base, bytestring, composite-base, hpack, hspec
, lens, opaleye, postgresql-simple, product-profunctors
, profunctors, QuickCheck, stdenv, template-haskell, text, vinyl
}:
mkDerivation {
  pname = "composite-opaleye";
  version = "0.6.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring composite-base lens opaleye postgresql-simple
    product-profunctors profunctors template-haskell text vinyl
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base bytestring composite-base hspec lens opaleye postgresql-simple
    product-profunctors profunctors QuickCheck template-haskell text
    vinyl
  ];
  prePatch = "hpack";
  homepage = "https://github.com/ConferHealth/composite#readme";
  description = "Opaleye SQL for Frames records";
  license = stdenv.lib.licenses.bsd3;
}

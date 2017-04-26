{ mkDerivation, aeson, aeson-better-errors, aeson-qq, base
, composite-base, containers, contravariant, generic-deriving
, hashable, hspec, lens, profunctors, QuickCheck, scientific
, stdenv, tagged, template-haskell, text, time
, unordered-containers, vector, vinyl
}:
mkDerivation {
  pname = "composite-aeson";
  version = "0.3.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-better-errors base composite-base containers
    contravariant generic-deriving hashable lens profunctors scientific
    tagged template-haskell text time unordered-containers vector vinyl
  ];
  testHaskellDepends = [
    aeson aeson-better-errors aeson-qq base composite-base containers
    contravariant generic-deriving hashable hspec lens profunctors
    QuickCheck scientific tagged template-haskell text time
    unordered-containers vector vinyl
  ];
  homepage = "https://github.com/ConferHealth/composite#readme";
  description = "JSON for Vinyl/Frames records";
  license = stdenv.lib.licenses.bsd3;
}

{ mkDerivation, base, composite-aeson, composite-base, hpack, hspec
, insert-ordered-containers, lens, lib, QuickCheck, swagger2
, template-haskell, text, vinyl
}:
mkDerivation {
  pname = "composite-swagger";
  version = "0.7.6.0";
  src = ./.;
  libraryHaskellDepends = [
    base composite-base insert-ordered-containers lens swagger2
    template-haskell text vinyl
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base composite-aeson composite-base hspec insert-ordered-containers
    lens QuickCheck swagger2 template-haskell text vinyl
  ];
  prePatch = "hpack";
  homepage = "https://github.com/ConferOpenSource/composite#readme";
  description = "Swagger for Vinyl records";
  license = lib.licenses.bsd3;
}

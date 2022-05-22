{ mkDerivation, aeson-better-errors, base, composite-aeson, hpack
, lib, mtl, refined
}:
mkDerivation {
  pname = "composite-aeson-refined";
  version = "0.7.6.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson-better-errors base composite-aeson mtl refined
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/ConferOpenSource/composite#readme";
  description = "composite-aeson support for Refined from the refined package";
  license = lib.licenses.bsd3;
}

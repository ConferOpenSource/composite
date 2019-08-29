{ mkDerivation, aeson-better-errors, base, composite-aeson, hpack
, mtl, refined, stdenv
}:
mkDerivation {
  pname = "composite-aeson-refined";
  version = "0.6.5.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson-better-errors base composite-aeson mtl refined
  ];
  libraryToolDepends = [ hpack ];
  preConfigure = "hpack";
  homepage = "https://github.com/ConferHealth/composite#readme";
  description = "composite-aeson support for Refined from the refined package";
  license = stdenv.lib.licenses.bsd3;
}

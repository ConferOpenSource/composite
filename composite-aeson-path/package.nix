{ mkDerivation, aeson-better-errors, base, composite-aeson, hpack
, mtl, refined, stdenv
}:
mkDerivation {
  pname = "composite-aeson-path";
  version = "0.7.2.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson-better-errors base composite-aeson mtl refined
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/ConferOpenSource/composite#readme";
  description = "composite-aeson support for Refined from the refined package";
  license = stdenv.lib.licenses.bsd3;
}

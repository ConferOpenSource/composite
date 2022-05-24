{ mkDerivation, base, composite-aeson, hpack, lib, path }:
mkDerivation {
  pname = "composite-aeson-path";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base composite-aeson path ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/ConferOpenSource/composite#readme";
  description = "Formatting data for the path library";
  license = lib.licenses.bsd3;
}

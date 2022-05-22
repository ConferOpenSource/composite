{ mkDerivation, base, composite-base, hashable, hpack, lib }:
mkDerivation {
  pname = "composite-hashable";
  version = "0.7.6.0";
  src = ./.;
  libraryHaskellDepends = [ base composite-base hashable ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/ConferOpenSource/composite#readme";
  description = "Orphan hashable instances";
  license = lib.licenses.bsd3;
}

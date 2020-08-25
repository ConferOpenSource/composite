{ mkDerivation, base, composite-base, hashable, hpack, stdenv }:
mkDerivation {
  pname = "composite-hashable";
  version = "0.7.3.0";
  src = ./.;
  libraryHaskellDepends = [ base composite-base hashable ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/ConferOpenSource/composite#readme";
  description = "Orphan hashable instances";
  license = stdenv.lib.licenses.bsd3;
}

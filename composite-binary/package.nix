{ mkDerivation, base, binary, composite-base, hpack, stdenv }:
mkDerivation {
  pname = "composite-binary";
  version = "0.7.3.0";
  src = ./.;
  libraryHaskellDepends = [ base binary composite-base ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/ConferOpenSource/composite#readme";
  description = "Orphan binary instances";
  license = stdenv.lib.licenses.bsd3;
}

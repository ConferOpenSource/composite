{ mkDerivation, base, composite-base, ekg-core, hpack, lens, stdenv
, text, vinyl
}:
mkDerivation {
  pname = "composite-ekg";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base composite-base ekg-core lens text vinyl
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/ConferOpenSource/composite#readme";
  description = "EKG Metrics for Vinyl records";
  license = stdenv.lib.licenses.bsd3;
}

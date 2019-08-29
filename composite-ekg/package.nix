{ mkDerivation, base, composite-base, ekg-core, hpack, lens, stdenv
, text, vinyl
}:
mkDerivation {
  pname = "composite-ekg";
  version = "0.6.5.0";
  src = ./.;
  libraryHaskellDepends = [
    base composite-base ekg-core lens text vinyl
  ];
  libraryToolDepends = [ hpack ];
  preConfigure = "hpack";
  homepage = "https://github.com/ConferHealth/composite#readme";
  description = "EKG Metrics for Vinyl/Frames records";
  license = stdenv.lib.licenses.bsd3;
}

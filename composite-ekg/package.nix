{ mkDerivation, base, composite-base, ekg-core, lens, stdenv, text
, vinyl
}:
mkDerivation {
  pname = "composite-ekg";
  version = "0.5.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base composite-base ekg-core lens text vinyl
  ];
  homepage = "https://github.com/ConferHealth/composite#readme";
  description = "EKG Metrics for Vinyl/Frames records";
  license = stdenv.lib.licenses.bsd3;
}

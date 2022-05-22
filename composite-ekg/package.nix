{ mkDerivation, base, composite-base, ekg-core, hpack, lens, lib
, text, vinyl
}:
mkDerivation {
  pname = "composite-ekg";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base composite-base ekg-core lens text vinyl
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/ConferOpenSource/composite#readme";
  description = "EKG Metrics for Vinyl records";
  license = lib.licenses.bsd3;
}

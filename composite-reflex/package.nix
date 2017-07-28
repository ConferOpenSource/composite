{ mkDerivation, base, composite-base, data-default, dependent-map
, dependent-sum, http-api-data, lens, reflex, reflex-dom, stdenv
, text, vinyl
}:
mkDerivation {
  pname = "composite-reflex";
  version = "0.5.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base composite-base data-default dependent-map dependent-sum
    http-api-data lens reflex reflex-dom text vinyl
  ];
  homepage = "https://github.com/ConferHealth/composite#readme";
  description = "Utilities for using composite records and corecords with Reflex";
  license = stdenv.lib.licenses.bsd3;
}

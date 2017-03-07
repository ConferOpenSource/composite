{ mkDerivation, aeson, aeson-better-errors, base, basic-prelude
, composite-base, containers, contravariant, generic-deriving, lens
, profunctors, scientific, stdenv, text, unordered-containers
, vinyl
}:
mkDerivation {
  pname = "composite-aeson";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-better-errors base basic-prelude composite-base
    containers contravariant generic-deriving lens profunctors
    scientific text unordered-containers vinyl
  ];
  homepage = "https://github.com/ConferHealth/composite#readme";
  description = "JSON for Vinyl/Frames records";
  license = stdenv.lib.licenses.bsd3;
}

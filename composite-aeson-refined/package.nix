{ mkDerivation, base, composite-aeson, refined, stdenv }:
mkDerivation {
  pname = "composite-aeson-refined";
  version = "0.4.0.0";
  src = ./.;
  libraryHaskellDepends = [ base composite-aeson refined ];
  homepage = "https://github.com/ConferHealth/composite#readme";
  description = "composite-aeson support for Refined from the refined package";
  license = stdenv.lib.licenses.bsd3;
}

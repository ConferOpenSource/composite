{ mkDerivation, base, composite-base, insert-ordered-containers
, lens, swagger2, template-haskell, text, vinyl
}:
mkDerivation {
  pname = "composite-swagger";
  version = "0.5.3.0";
  src = ./.;
  libraryHaskellDepends = [
    base composite-base insert-ordered-containers lens swagger2
    template-haskell text vinyl
  ];
  homepage = "https://github.com/ConferHealth/composite#readme";
  description = "Swagger integration for Vinyl/Frames records";
  license = stdenv.lib.licenses.bsd3;
}

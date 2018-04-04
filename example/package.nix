{ mkDerivation, aeson, base, bytestring, classy-prelude
, composite-aeson, composite-base, composite-ekg, composite-opaleye
, composite-swagger, configurator, ekg, ekg-core, exceptions
, fast-logger, http-api-data, insert-ordered-containers, lens
, monad-control, monad-logger, mtl, opaleye, postgresql-simple
, product-profunctors, profunctors, resource-pool, servant
, servant-server, servant-swagger, servant-swagger-ui, stdenv
, swagger2, text, vinyl, warp
}:
mkDerivation {
  pname = "myawesomeserver";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring classy-prelude composite-aeson composite-base
    composite-ekg composite-opaleye composite-swagger configurator ekg
    ekg-core exceptions fast-logger http-api-data
    insert-ordered-containers lens monad-control monad-logger mtl
    opaleye postgresql-simple product-profunctors profunctors
    resource-pool servant servant-server servant-swagger
    servant-swagger-ui swagger2 text vinyl warp
  ];
  executableHaskellDepends = [
    aeson base bytestring classy-prelude composite-aeson composite-base
    composite-ekg composite-opaleye composite-swagger configurator ekg
    ekg-core exceptions fast-logger http-api-data
    insert-ordered-containers lens monad-control monad-logger mtl
    opaleye postgresql-simple product-profunctors profunctors
    resource-pool servant servant-server servant-swagger
    servant-swagger-ui swagger2 text vinyl warp
  ];
  homepage = "https://github.com/ConferHealth/composite#readme";
  description = "It's a server!";
  license = stdenv.lib.licenses.bsd3;
}

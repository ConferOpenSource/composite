{ mkDerivation, aeson, base, bytestring, classy-prelude
, composite-aeson, composite-base, composite-ekg, composite-opaleye
, configurator, ekg, ekg-core, fast-logger, http-api-data, lens
, monad-logger, mtl, opaleye, postgresql-simple
, product-profunctors, profunctors, resource-pool, servant
, servant-server, stdenv, vinyl, warp
}:
mkDerivation {
  pname = "myawesomeserver";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring classy-prelude composite-aeson composite-base
    composite-ekg composite-opaleye configurator ekg ekg-core
    fast-logger http-api-data lens monad-logger mtl opaleye
    postgresql-simple product-profunctors profunctors resource-pool
    servant servant-server vinyl warp
  ];
  executableHaskellDepends = [
    aeson base bytestring classy-prelude composite-aeson composite-base
    composite-ekg composite-opaleye configurator ekg ekg-core
    fast-logger http-api-data lens monad-logger mtl opaleye
    postgresql-simple product-profunctors profunctors resource-pool
    servant servant-server vinyl warp
  ];
  homepage = "https://github.com/ConferHealth/composite#readme";
  description = "It's a server!";
  license = stdenv.lib.licenses.bsd3;
}

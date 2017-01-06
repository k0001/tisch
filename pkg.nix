{ mkDerivation, aeson, base, bytestring, case-insensitive
, exceptions, lens, mtl, opaleye, postgresql-simple
, product-profunctors, profunctors, scientific, semigroups, singletons, stdenv ,
tagged, text, time, transformers, uuid
}:
mkDerivation {
  pname = "tisch";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive exceptions lens mtl opaleye
    postgresql-simple product-profunctors profunctors scientific semigroups
    singletons tagged text time transformers uuid
  ];
  testHaskellDepends = [
    base lens mtl opaleye postgresql-simple time
  ];
  homepage = "https://github.com/k0001/tisch";
  description = "Opaleye's sugar on top";
  license = stdenv.lib.licenses.bsd3;
}

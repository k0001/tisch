{ mkDerivation, aeson, base, bytestring, case-insensitive
, exceptions, lens, mtl, opaleye, postgresql-simple
, product-profunctors, profunctors, semigroups, singletons, stdenv
, text, time, transformers, uuid
}:
mkDerivation {
  pname = "opaleye-sot";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive exceptions lens mtl
    opaleye postgresql-simple product-profunctors profunctors
    semigroups singletons text time transformers uuid
  ];
  testHaskellDepends = [
    base lens mtl opaleye postgresql-simple time
  ];
  homepage = "https://github.com/k0001/opaleye-sot";
  description = "Opaleye's sugar on top";
  license = stdenv.lib.licenses.bsd3;
}

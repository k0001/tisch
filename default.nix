{ mkDerivation, base, exceptions, HList, lens, opaleye, product-profunctors,
  profunctors, singletons, stdenv }:
mkDerivation {
  pname = "opaleye-sot";
  version = "0.1";
  src = ./.;
  configureFlags = [ "--ghc-options=-O0" ];
  buildDepends = [ base exceptions HList lens opaleye singletons
                   product-profunctors profunctors ];
  homepage = "https://github.com/k0001/opaleye-sot";
  description = "Opaleye's sugar on top";
  license = stdenv.lib.licenses.bsd3;
}

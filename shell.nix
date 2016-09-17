{ nixpkgs ? ./nixpkgs.git
, compiler ? "ghc801"
}:



let
  inherit (import nixpkgs {}) pkgs;
  hs = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      mkDerivation = args:
        super.mkDerivation (args // {
          enableLibraryProfiling = true;
          configureFlags = (args.configureFlags or []) ++ [
            "--enable-optimization=2"
            "--ghc-options=-fprof-auto-calls"
            "--ghc-options=-fprof-auto-exported"
            "--ghc-options=-fprof-auto-top"
            "--ghc-options=-fprof-cafs"
          ];
        });
      tisch = self.callPackage ./default.nix {};
      opaleye = pkgs.haskell.lib.overrideCabal super.opaleye (drv: {
        src = pkgs.fetchFromGitHub {
          owner = "k0001";
          repo = "haskell-opaleye";
          rev = "56487a849c9b22410c14c47df76d41613067245a";
          sha256 = "030d1ws25i5ry4403v8f14lkjzcr7b734jk62i4fss7gimvix56k";
        };
      });
    };
  };
  drv = hs.tisch;
in
  if pkgs.lib.inNixShell then drv.env else drv

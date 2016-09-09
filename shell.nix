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
      opaleye-sot = self.callPackage ./default.nix {};
      opaleye = pkgs.haskell.lib.overrideCabal super.opaleye (drv: {
        src = pkgs.fetchFromGitHub {
          owner = "tomjaguarpaw";
          repo = "haskell-opaleye";
          rev = "3214468";
          sha256 = "1msxvp47jxsylh0ls5az7p8mh1xxdz6yh81swqbf0qyp9myqxwr0";
        };
      });
    };
  };
  drv = hs.opaleye-sot;
in
  if pkgs.lib.inNixShell then drv.env else drv

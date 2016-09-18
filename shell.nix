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
          owner = "tomjaguarpaw";
          repo = "haskell-opaleye";
          rev = "2844e4a221d14cfb1047630b52ba1e9bf5992f5a";
          sha256 = "188c6g2l5dqn037vh1k8r96zwlljr2f6ikakipcxr08kcwb7awbm";

        };
      });
    };
  };
  drv = hs.tisch;
in
  if pkgs.lib.inNixShell then drv.env else drv

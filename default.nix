let
_pkgs0 = import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "4aadb9beb345898c7f7b06090b163e7f3debea5a";
  sha256 = "1dka2knhi8pfb83iwph7chldys1df95dc3z2v38cqzy4m06qjir9";
}) {};

in
{ pkgs ? _pkgs0
, compiler ? "ghc801"
}:

let
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
      tisch = pkgs.haskell.lib.overrideCabal (self.callPackage ./pkg.nix {}) (drv: {
        enableExecutableProfiling = true;
        enableSplitObjs = false;
      });
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

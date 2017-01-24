let
_pkgs0 = import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "c78022aad18bfed0d13e0191f58045296288e79a";
  sha256 = "1ngrbpks56kx58n3iwwf5jwlj6mdv16d0xnzrnb1hcgham2k2dbv";
}) {};

in
{ pkgs ? _pkgs0
, compiler ? "ghc802"
}:

let
  inherit (pkgs) stdenv;
  hs = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      #mkDerivation = args:
      #  super.mkDerivation (args // {
      #    enableLibraryProfiling = true;
      #    configureFlags = (args.configureFlags or []) ++ [
      #      "--enable-optimization=2"
      #      "--ghc-options=-fprof-auto-calls"
      #      "--ghc-options=-fprof-auto-exported"
      #      "--ghc-options=-fprof-auto-top"
      #      "--ghc-options=-fprof-cafs"
      #      (stdenv.lib.optionalString stdenv.isLinux
      #        "--ghc-options=-split-sections")
      #    ];
      #  });
      tisch = pkgs.haskell.lib.overrideCabal (self.callPackage ./pkg.nix {}) (drv: {
      #  enableExecutableProfiling = true;
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

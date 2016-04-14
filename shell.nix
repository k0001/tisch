{ nixpkgs ? import <nixpkgs> {}, compiler ? "lts-5_1" }:

let
  inherit (nixpkgs) pkgs;
  drv = hs.opaleye-sot;
  hs = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      opaleye-sot = self.callPackage ./default.nix {};
      opaleye = pkgs.haskell.lib.overrideCabal super.opaleye (drv: {
        src = pkgs.fetchFromGitHub {
          owner = "tomjaguarpaw";
          repo = "haskell-opaleye";
          rev = "bca7d214c0d14eed8e6d9b9166d7ce02e2865b1b";
          sha256 = "0r2r3adxzbz3n7sk7zzfrfyndq9dn5m847xiihmsg5lycm15p0nm";
        };
      });
    };
  };
in
  if pkgs.lib.inNixShell then drv.env else drv

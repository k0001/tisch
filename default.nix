let
_pkgs0 = import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "870cf05b6fc2c877d3b1cfa045b8b6d7129b2f42";
  sha256 = "0b0q6aqar3xj2ac0x2prh6bmmxd1ndx85fw2bvbaak8sgbc4xf2z";
}) {};

in
{ pkgs ? _pkgs0
, compiler ? "ghc802"
}:

let
  inherit (pkgs) stdenv;
  hs = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      tisch = self.callPackage ./pkg.nix {};
    };
  };
  drv = hs.tisch;
in
  if pkgs.lib.inNixShell then drv.env else drv

{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:
let
  inherit (nixpkgs) pkgs;
  drv = import ./. { inherit nixpkgs compiler doBenchmark; };
in
  if pkgs.lib.inNixShell then drv.env else drv

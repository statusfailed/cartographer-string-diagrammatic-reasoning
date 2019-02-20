{ pkgs ? import ./nixpkgs.nix }:
let
  inherit (pkgs.haskell.packages) ghcjs;

  drv = ghcjs.callCabal2nix "cartographer-editor" ./. {};

  final = pkgs.runCommand "cartographer-editor" {} ''
    mkdir $out
    cp ${drv}/bin/cartographer-editor.jsexe/all.js $out/all.js
    cp ${./html-src/index.html} $out/index.html
  '';
in
  if pkgs.lib.inNixShell then drv.env else final

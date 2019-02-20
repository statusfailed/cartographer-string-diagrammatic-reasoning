let
  config = { };

  pkgs = import <nixpkgs> { inherit config; };

in
  { cartographer = pkgs.haskellPackages.callPackage ./cartographer.nix { };
  }

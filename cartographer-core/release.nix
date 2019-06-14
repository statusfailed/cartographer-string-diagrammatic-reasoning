let
  config = { };

  pkgs = import <nixpkgs> { inherit config; };

in
  { cartographer-core = pkgs.haskellPackages.callPackage ./cartographer-core.nix { };
  }

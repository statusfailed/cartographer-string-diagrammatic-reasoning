let
  pkgs = import <nixpkgs> { };

in
  { cartographer-core = pkgs.profiledHaskellPackages.callPackage ./cartographer-core.nix { };
  }

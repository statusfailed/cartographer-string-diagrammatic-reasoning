let
  pkgs = import <nixpkgs> { };

in
  { cartographer-core = pkgs.haskellPackages.callPackage ./cartographer-core.nix { };
}

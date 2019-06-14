{ mkDerivation, array, base, bimap, containers, linear, logict, mtl
, reflection, stdenv, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "cartographer-core";
  version = "0.1.0.2";
  src = ./.;
  libraryHaskellDepends = [
    array base bimap containers linear logict mtl reflection
  ];
  testHaskellDepends = [ base tasty tasty-quickcheck ];
  license = stdenv.lib.licenses.mit;
}

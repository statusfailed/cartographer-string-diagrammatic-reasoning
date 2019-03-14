{ mkDerivation, array, base, containers, linear, bimap, mtl, logict, stdenv }:
mkDerivation {
  pname = "cartographer";
  version = "0.1.0.2";
  src = ../cartographer;
  libraryHaskellDepends = [ array base containers linear bimap mtl logict ];
  license = stdenv.lib.licenses.mit;
}

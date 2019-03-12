{ mkDerivation, array, base, containers, linear, bimap, mtl, logict, stdenv }:
mkDerivation {
  pname = "cartographer";
  version = "0.1.0.0";
  src = ../cartographer;
  libraryHaskellDepends = [ array base containers linear bimap mtl logict ];
  license = stdenv.lib.licenses.mit;
}

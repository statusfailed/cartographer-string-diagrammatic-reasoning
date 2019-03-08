{ mkDerivation, array, base, containers, linear, bimap, mtl, stdenv }:
mkDerivation {
  pname = "cartographer";
  version = "0.1.0.0";
  src = ../cartographer;
  libraryHaskellDepends = [ array base containers linear bimap mtl ];
  license = stdenv.lib.licenses.mit;
}

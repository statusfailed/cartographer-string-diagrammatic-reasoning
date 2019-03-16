{ mkDerivation, array, base, containers, linear, bimap, mtl, logict, reflection, stdenv }:
mkDerivation {
  pname = "cartographer";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ array base containers linear bimap mtl logict reflection ];
  license = stdenv.lib.licenses.mit;
}

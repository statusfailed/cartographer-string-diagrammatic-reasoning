{ mkDerivation, array, base, containers, linear, stdenv }:
mkDerivation {
  pname = "cartographer";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ array base containers linear ];
  license = stdenv.lib.licenses.mit;
}

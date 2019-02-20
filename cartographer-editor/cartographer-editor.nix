{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "cartographer-editor";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base miso containers linear cartographer ];
  license = stdenv.lib.licenses.mit;
}

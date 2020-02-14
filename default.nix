{ mkDerivation, base, pure, pure-cond, pure-prop, stdenv
}:
mkDerivation {
  pname = "pure-responsive";
  version = "0.8.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base pure pure-cond pure-prop
  ];
  license = stdenv.lib.licenses.bsd3;
}

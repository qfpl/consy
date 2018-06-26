{ mkDerivation, base, criterion, inspection-testing, lens, stdenv
, text
}:
mkDerivation {
  pname = "consy";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens text ];
  testHaskellDepends = [ base criterion inspection-testing text ];
  license = stdenv.lib.licenses.bsd3;
}

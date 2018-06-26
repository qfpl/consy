{ mkDerivation, base, containers, criterion, inspection-testing
, lens, stdenv, text
}:
mkDerivation {
  pname = "consy";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers lens text ];
  testHaskellDepends = [
    base containers criterion inspection-testing text
  ];
  license = stdenv.lib.licenses.bsd3;
}

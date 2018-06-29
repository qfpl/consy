{ mkDerivation, base, bytestring, containers, criterion
, inspection-testing, lens, stdenv, text
}:
mkDerivation {
  pname = "consy";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring containers lens text ];
  testHaskellDepends = [
    base bytestring containers criterion inspection-testing lens text
  ];
  license = stdenv.lib.licenses.bsd3;
}

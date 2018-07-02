{ mkDerivation, base, bytestring
, containers, criterion, inspection-testing, lens, stdenv, text
, vector
}:
mkDerivation {
  pname = "consy";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers inspection-testing lens text vector
  ];
  testHaskellDepends = [ criterion ];
  benchmarkHaskellDepends = [
    base bytestring containers criterion
    inspection-testing lens text
  ];
  license = stdenv.lib.licenses.bsd3;
}

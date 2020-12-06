{ mkDerivation, aeson, base, bytestring, containers, hspec
, hspec-discover, stdenv, text, vector
}:

mkDerivation {
  pname = "aeson-tiled";
  version = "0.0.0.1";
  sha256 = "147rm19czz8bvzmjj4mmcvjyz02mr3cisyprzdfpx1q5msi7aghk";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers text vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson base bytestring hspec hspec-discover
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/schell/aeson-tiled#readme";
  description = "Aeson instances for the Tiled map editor";
  license = stdenv.lib.licenses.bsd3;
}

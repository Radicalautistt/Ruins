{ mkDerivation, aeson, apecs, apecs-physics, array, async, base
, bytestring, directory, filepath, hashable, lens, linear, managed
, mtl, sdl2, sdl2-mixer, sdl2-ttf, stdenv, template-haskell, text
, transformers, unordered-containers, vector, megaparsec, text-short
, OpenGL, sdl2-image
}:

mkDerivation {
  pname = "ruins";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson apecs apecs-physics array async base bytestring directory
    filepath hashable lens linear managed mtl sdl2 sdl2-mixer sdl2-ttf
    template-haskell text transformers unordered-containers vector megaparsec
    text-short OpenGL sdl2-image
  ];
  homepage = "https://github.com/Radicalautistt/Ruins";
  description = "A Haskell remake of the RUINS section from Undertale.";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}

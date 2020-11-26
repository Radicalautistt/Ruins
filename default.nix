let unstableTarball  = fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz;

    pkgs             = import <nixpkgs> {};
    unstable         = import unstableTarball {};

    inherit (unstable) haskellPackages;

    haskellDependencies = p: with p; [ lens
                                       aeson
                                       apecs
                                       apecs-physics
                                       stm
                                       async
                                       unordered-containers
                                       linear
                                       sdl2
                                       sdl2-mixer
                                       sdl2-ttf
                                       managed
                                       unliftio
                                       ];

    ghc = haskellPackages.ghcWithPackages haskellDependencies;

in  pkgs.stdenv.mkDerivation {

    name = "undertale-dev-env";

    buildInputs = [ ghc
                    pkgs.zlib
                    pkgs.cabal2nix
                    haskellPackages.ghcid
                    haskellPackages.hlint
                    haskellPackages.hp2pretty
                    haskellPackages.cabal-install ];

    LD_LIBRARY_PATH="${pkgs.openssl.out}/lib;${pkgs.zlib}/lib";

 }

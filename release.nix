# stolen from https://github.com/cblp/hmm/blob/master/release.nix
let
  config = {
    packageOverrides = nixpkgs: rec {
      haskellPackages = nixpkgs.haskellPackages.override {
        overrides = new: old: rec {
        ruins = new.callPackage ./ruins.nix {};
        };
      };
    };
  };

  nixpkgs =
    import (fetchTarball
      https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) { inherit config; };

in {
  ruins = nixpkgs.haskellPackages.ruins;
}

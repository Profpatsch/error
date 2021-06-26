{ pkgs ? import <nixpkgs> {} }:

let error = pkgs.haskellPackages.callCabal2nix "error" ./error.cabal {};

in pkgs.haskellPackages.shellFor {
  packages = p: [
    error
  ];
  buildInputs = [
    pkgs.cabal-install
  ];
}

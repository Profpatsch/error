{ pkgs ? import <nixpkgs> {}}:

pkgs.haskellPackages.shellFor {
  packages = hps: [
    (hps.callCabal2nix "error" (pkgs.nix-gitignore.gitignoreSource [".git"] ./.) {})
  ];
  buildInputs = [
    pkgs.cabal-install
    pkgs.haskell-language-server
  ];
}

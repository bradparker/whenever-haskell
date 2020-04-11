{
  nixpkgs ? import ./nixpkgs.nix {
    overlays = [(import ./overlay.nix)];
  }
}:
  nixpkgs.haskellPackages.shellFor {
    packages = p: [ p.whenever-server p.whenever-core ];
    nativeBuildInputs = with nixpkgs; [ cabal-install ];
  }

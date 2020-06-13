{
  nixpkgs ? import ./nixpkgs.nix {
    overlays = [
      (import ./overlay.nix)
    ];
  }
}:
  nixpkgs.haskellPackages.shellFor {
    packages = p: with p; [
      cryptonite-srp
      whenever-core
      whenever-database
      whenever-server
    ];
    nativeBuildInputs = with nixpkgs; [
      cabal-install
      direnv
      docker-compose
      postgresql
    ];
  }

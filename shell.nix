{
  nixpkgs ? import ./nixpkgs.nix {
    overlays = [
      (import ./nix/hasql-pool-overlay.nix)
      (import ./overlay.nix)
    ];
  }
}:
  nixpkgs.haskellPackages.shellFor {
    packages = p: with p; [
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

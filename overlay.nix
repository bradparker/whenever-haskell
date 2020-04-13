self: super: {
  haskellPackages = super.haskellPackages.extend (
    super.haskell.lib.packageSourceOverrides {
      whenever-core = ./core;
      whenever-server = ./server;
      whenever-database = ./database;
    }
  );
}

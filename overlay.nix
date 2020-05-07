self: super: {
  haskellPackages = super.haskellPackages.extend (
    super.haskell.lib.packageSourceOverrides {
      whenever-core = ./core;
      cryptonite-srp = ./cryptonite-srp;
      whenever-database = ./database;
      whenever-server = ./server;
    }
  );
}

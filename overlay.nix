self: super: {
  haskellPackages = super.haskellPackages.extend (
    super.haskell.lib.packageSourceOverrides {
      cryptonite-srp = ./cryptonite-srp;
      whenever-core = ./core;
      whenever-database = ./database;
      whenever-server = ./server;
      whenever-tui = ./tui;
    }
  );
}

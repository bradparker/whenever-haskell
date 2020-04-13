self: super: {
  haskellPackages = super.haskellPackages.extend (
    hself: hsuper: {
      hasql-pool = super.haskell.lib.dontCheck hsuper.hasql-pool;
    }
  );
}

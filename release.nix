{
  nixpkgs ? import ./nixpkgs.nix {
    overlays = [(import ./overlay.nix)];
  }
}:
{
  whenever-server = nixpkgs.dockerTools.buildImage {
    name = "whenever";
    tag = "latest";
    contents = with nixpkgs; [
      haskellPackages.whenever-server
      busybox
    ];
    config = {
      Cmd = ["/bin/whenever"];
    };
  };
}

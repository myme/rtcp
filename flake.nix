{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "xchg - Exchange stuff between things using WebRTC";
  inputs.nixpkgs.url = "nixpkgs";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in {
      overlay = (final: prev: {
        xchg-server = final.haskellPackages.callCabal2nix "xchg-server" ./server {};
      });
      packages = forAllSystems (system: {
         xchg-server = nixpkgsFor.${system}.xchg-server;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.xchg-server);
      checks = self.packages;
      devShell = forAllSystems (system:
        let pkgs = nixpkgsFor.${system};
            haskellPackages = pkgs.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.xchg-server];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            pkgs.nodejs-16_x
            haskell-language-server
            ghcid
            cabal-install
          ];
        });
    };
}

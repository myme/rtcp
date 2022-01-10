{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "xchg - Exchange stuff between things using WebRTC";

  inputs.nixpkgs.url = "nixpkgs";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        });

      # https://www.reddit.com/r/NixOS/comments/ce9cmu/comment/eu1c7sh/?utm_source=share&utm_medium=web2x&context=3
      # https://gist.github.com/adisbladis/2a44cded73e048458a815b5822eea195
      mergeEnvs = pkgs: envs:
        pkgs.mkShell (builtins.foldl' (a: v: {
          buildInputs = a.buildInputs ++ v.buildInputs;
          nativeBuildInputs = a.nativeBuildInputs ++ v.nativeBuildInputs;
          propagatedBuildInputs = a.propagatedBuildInputs
            ++ v.propagatedBuildInputs;
          propagatedNativeBuildInputs = a.propagatedNativeBuildInputs
            ++ v.propagatedNativeBuildInputs;
          shellHook = a.shellHook + "\n" + v.shellHook;
        }) (pkgs.mkShell { }) envs);

    in {
      overlay = (final: prev: {
        xchg = (final.callPackage ./. { } // {
          server = final.callPackage ./server { };
          frontend = final.callPackage ./frontend { };
        });
      });
      apps =
        forAllSystems (system: { dev = nixpkgsFor.${system}.xchg.dev; });
      packages = forAllSystems (system:
        let pkgs = nixpkgsFor.${system};
        in {
          image = pkgs.xchg.image;
          server = pkgs.xchg.server.server;
          static = pkgs.xchg.frontend.static;
        });
      defaultPackage = forAllSystems (system: self.packages.${system}.image);
      checks = self.packages;
      devShell = forAllSystems (system:
        mergeEnvs nixpkgsFor.${system}
        (with self.devShells.${system}; [ frontend server ]));
      devShells = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
          haskellPackages = pkgs.haskellPackages;
        in {
          frontend = pkgs.xchg.frontend.shell;
          server = pkgs.xchg.server.shell;
        });
    };
}

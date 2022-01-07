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
        xchg-frontend = final.callPackage ./frontend { nodejs = final.nodejs; };
        xchg-server = final.haskellPackages.callCabal2nix "xchg-server" ./server {};
      });
      apps = forAllSystems (system: {
        dev =
          let pkgs = nixpkgsFor.${system};
          in pkgs.writeShellScriptBin "dev" ''
            rm -rf ./node_modules
            ln -s ${pkgs.xchg-frontend.nodeDependencies}/lib/node_modules ./node_modules
            export PATH="${pkgs.xchg-frontend.nodeDependencies}/bin:$PATH"
            npx concurrently \
                -n FE,BE \
                -c green,red \
                "cd frontend && npm run frontend" \
                "cd server && ghcid -r Main"
          '';
      });
      packages = forAllSystems (system: {
        frontend =
          let pkgs = nixpkgsFor.${system};
          in pkgs.stdenv.mkDerivation {
            name = "xchg-frontend";
            src = ./frontend;
            buildInputs = [pkgs.nodejs];
            buildPhase = ''
              ln -s ${pkgs.xchg-frontend.nodeDependencies}/lib/node_modules ./node_modules
              export PATH="${pkgs.xchg-frontend.nodeDependencies}/bin:$PATH"
              npm run build
            '';
            installPhase = ''
              cp -r dist $out/
            '';
          };
        server = nixpkgsFor.${system}.xchg-server;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.xchg);
      checks = self.packages;
      devShell =
        # https://www.reddit.com/r/NixOS/comments/ce9cmu/comment/eu1c7sh/?utm_source=share&utm_medium=web2x&context=3
        # https://gist.github.com/adisbladis/2a44cded73e048458a815b5822eea195
        forAllSystems (system:
          let pkgs = nixpkgsFor.${system};
              mergeEnvs = envs: pkgs.mkShell (builtins.foldl' (a: v: {
                buildInputs = a.buildInputs ++ v.buildInputs;
                nativeBuildInputs = a.nativeBuildInputs ++ v.nativeBuildInputs;
                propagatedBuildInputs = a.propagatedBuildInputs ++ v.propagatedBuildInputs;
                propagatedNativeBuildInputs = a.propagatedNativeBuildInputs ++ v.propagatedNativeBuildInputs;
                shellHook = a.shellHook + "\n" + v.shellHook;
              }) (pkgs.mkShell {}) envs);
          in mergeEnvs (with self.devShells.${system}; [frontend server])
        );
      devShells = forAllSystems (system:
        let pkgs = nixpkgsFor.${system};
            haskellPackages = pkgs.haskellPackages;
        in {
          frontend = pkgs.xchg-frontend.shell.override {
            buildInputs = [pkgs.nodePackages.node2nix];
          };
          server = haskellPackages.shellFor {
            packages = p: [pkgs.xchg-server];
            withHoogle = true;
            buildInputs = with haskellPackages; [
              haskell-language-server
              ghcid
              cabal-install
            ];
          };
        });
    };
}

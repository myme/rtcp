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

      # Docker image
      image = pkgs:
        pkgs.dockerTools.buildLayeredImage {
          name = "xchg";
          tag = "latest";
          contents = with pkgs; [ xchg-server xchg-static ];
          config = {
            Cmd = [ "xchg-server" ];
            ExposedPorts = { "8000/tcp" = { }; };
          };
        };

      # Launch development server
      dev = pkgs:
        pkgs.writeShellScriptBin "dev" ''
          rm -rf ./node_modules
          ln -s ${pkgs.xchg-node.nodeDependencies}/lib/node_modules ./node_modules
          export PATH="${pkgs.xchg-node.nodeDependencies}/bin:$PATH"
          npx concurrently \
              -n FE,BE \
              -c green,red \
              "cd frontend && npm run frontend" \
              "cd server && hpack && cabal build && ghcid -r Main"
        '';

      # Static frontend assets
      static = pkgs:
        pkgs.stdenv.mkDerivation {
          name = "xchg-frontend";
          src = ./frontend;
          buildInputs = [ pkgs.nodejs ];
          buildPhase = ''
            ln -s ${pkgs.xchg-node.nodeDependencies}/lib/node_modules ./node_modules
            export PATH="${pkgs.xchg-node.nodeDependencies}/bin:$PATH"
            npm run build
          '';
          installPhase = ''
            cp -r dist $out/
          '';
        };

    in {
      overlay = (final: prev: {
        xchg-node = final.callPackage ./frontend { nodejs = final.nodejs; };
        xchg-static = static final;
        xchg-server =
          final.haskellPackages.callCabal2nix "xchg-server" ./server { };
      });
      apps = forAllSystems (system: { dev = dev nixpkgsFor.${system}; });
      packages = forAllSystems (system:
        let pkgs = nixpkgsFor.${system};
        in {
          image = image pkgs;
          static = static pkgs;
          server = pkgs.xchg-server;
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
          frontend = pkgs.xchg-node.shell.override {
            buildInputs = [ pkgs.nodePackages.node2nix ];
          };
          server = haskellPackages.shellFor {
            packages = p: [ pkgs.xchg-server ];
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

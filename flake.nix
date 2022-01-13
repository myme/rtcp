{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "xchg - Exchange stuff between things using WebRTC";

  inputs.nixpkgs.url = "nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
        overlay = (final: prev: {
          xchg = (final.callPackage ./. { } // {
            server = final.callPackage ./server { };
            frontend = final.callPackage ./frontend { };
          });
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

      in rec {
        apps = { dev = pkgs.xchg.dev; };
        packages = {
          image = pkgs.xchg.image;
          server = pkgs.xchg.server.server;
          static = pkgs.xchg.frontend.static;
        };
        defaultPackage = packages.image;
        checks = packages;
        devShell = mergeEnvs pkgs (with devShells; [ frontend server ]);
        devShells = {
          frontend = pkgs.xchg.frontend.shell;
          server = pkgs.xchg.server.shell;
        };
      });
}

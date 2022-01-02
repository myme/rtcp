{
  description = "xchg - Exchange stuff between things using WebRTC";
  inputs.nixpkgs.url = "nixpkgs";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [];
      });
    in {
      devShell = forAllSystems (system:
        let pkgs = nixpkgsFor.${system};
        in pkgs.mkShell {
          buildInputs = with pkgs; [
            nodejs-16_x
            (python3.withPackages (ps: with ps; [
              aiohttp
              python-socketio
            ]))
          ];
        });
    };
}

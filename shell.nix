{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs-16_x
    (python3.withPackages (ps: with ps; [
      aiohttp
      python-socketio
    ]))
  ];
}

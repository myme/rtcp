{ stdenv, callPackage, nodejs, nodePackages, writeShellScriptBin }:
let
  node = callPackage ./nix { inherit nodejs; };
  node2nix = writeShellScriptBin "node2nix" ''
    ${nodePackages.node2nix}/bin/node2nix \
      --development \
      -l package-lock.json \
      -c ./nix/default.nix \
      -o ./nix/node-packages.nix \
      -e ./nix/node-env.nix
  '';
in {
  nodeDependencies = node.nodeDependencies;
  static = stdenv.mkDerivation {
    name = "xchg-frontend";
    src = ./.;
    buildInputs = [ nodejs ];
    buildPhase = ''
      ln -s ${node.nodeDependencies}/lib/node_modules ./node_modules
      export PATH="${node.nodeDependencies}/bin:$PATH"
      npm run build
    '';
    installPhase = ''
      cp -r dist $out/
    '';
  };
  shell = node.shell.override {
    buildInputs = [ node2nix ];
  };
}

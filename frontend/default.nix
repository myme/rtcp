{ stdenv, callPackage, nodejs, nodePackages }:
let
  node = callPackage ./nix { inherit nodejs; };
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
    buildInputs = [ nodePackages.node2nix ];
  };
}

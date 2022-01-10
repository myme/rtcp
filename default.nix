{ dockerTools, writeShellScriptBin, xchg }:

{
  # Docker image
  image = dockerTools.buildLayeredImage {
    name = "xchg";
    tag = "latest";
    contents = with xchg; [ server.server frontend.static ];
    config = {
      Cmd = [ "xchg-server" ];
      ExposedPorts = { "8000/tcp" = { }; };
    };
  };

  # Launch development server
  dev = writeShellScriptBin "dev" ''
    rm -rf ./node_modules
    ln -s ${xchg.frontend.nodeDependencies}/lib/node_modules ./node_modules
    export PATH="${xchg.frontend.nodeDependencies}/bin:$PATH"
    npx concurrently \
        -n FE,BE \
        -c green,red \
        "cd frontend && npm run frontend" \
        "cd server && hpack && cabal build && ghcid -r Main"
  '';
}

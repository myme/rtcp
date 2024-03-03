{ concurrently, dockerTools, writeShellScriptBin, rtcp }:

{
  # Docker image
  image = dockerTools.buildLayeredImage {
    name = "rtcp";
    tag = "latest";
    contents = with rtcp; [ server.server frontend.static ];
    config = {
      Cmd = [ "rtcp-server" "--host" "0.0.0.0" "--port" "8000" ];
      ExposedPorts = { "8000/tcp" = { }; };
    };
  };

  # Launch development server
  dev = writeShellScriptBin "dev" ''
    rm -rf ./node_modules
    ln -s ${rtcp.frontend.nodeDependencies}/lib/node_modules ./node_modules
    export PATH="${rtcp.frontend.nodeDependencies}/bin:$PATH"
    nix develop --command ${concurrently}/bin/concurrently \
      -n FE,BE \
      -c green,red \
      "cd frontend && npm start" \
      "cd server && hpack && cabal build && ghcid -r"
  '';
}

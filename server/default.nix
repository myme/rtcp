{ haskellPackages }:
rec {
  server = haskellPackages.callCabal2nix "rtcp-server" ./. {};
  shell = haskellPackages.shellFor {
    packages = p: [server];
    withHoogle = true;
    buildInputs = with haskellPackages; [
      haskell-language-server
      ghcid
      cabal-install
    ];
  };
}

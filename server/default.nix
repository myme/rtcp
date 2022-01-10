{ haskellPackages }:
rec {
  server = haskellPackages.callCabal2nix "xchg-server" ./. {};
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

{
  description = "mono-hackage";
  nixConfig.bash-prompt = "[nix(mono-hackage)] ";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/ed014c27f4d0ca772fb57d3b8985b772b0503bbd";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          # config.allowBroken = true;
        };
        packageName = "mono-hackage";
        haskellPackages = pkgs.haskellPackages;
        myPackage = haskellPackages.callCabal2nix packageName self { };

      in {
        defaultExe = pkgs.haskell.lib.justStaticExecutables myPackage;
        defaultPackage = myPackage;

        devShell = haskellPackages.shellFor {
          packages = p: [ myPackage ];

          buildInputs = with haskellPackages; [
            ghcid
            ormolu
            cabal-install
            hlint
            pkgs.haskell-language-server
          ];
        };
      });
}

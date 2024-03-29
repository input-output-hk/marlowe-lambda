{

  description = "Marlowe support for IOG's Contract-Lambda";

  inputs = {

    flake-utils.url = "github:numtide/flake-utils";

    haskell-nix.url = "github:input-output-hk/haskell.nix";

    nixpkgs.follows = "haskell-nix/nixpkgs-2205";

    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };

  };


  outputs = { self, nixpkgs, flake-utils, haskell-nix, CHaP }:

    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:

      let

        overlays = [ haskell-nix.overlay
          (final: prev: {
            marlowe-lambda =
              final.haskell-nix.project' {
                inputMap = {
                  "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
                };
                src = ./.;
                compiler-nix-name = "ghc8107";
                shell.tools = {
                  cabal                   = {};
                  ghcid                   = {};
                  haskell-language-server = {};
                  hie-bios                = {};
                  pointfree               = {};
                };
                shell.buildInputs = with pkgs; [
                  pkgs.haskellPackages.hlint  # FIXME: Move to "shell.tools".
                ];
              };
          })
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskell-nix) config; };

        flake = pkgs.marlowe-lambda.flake {
        };

      in

        flake // {
          defaultPackage = flake.packages."marlowe-lambda:exe:marlowe-lambda";
          hydraJobs = {
            marlowe-lambda = flake.packages."marlowe-lambda:exe:marlowe-lambda";
            marlowe-pipe = flake.packages."marlowe-lambda:exe:marlowe-pipe";
          };
        }

    );

}

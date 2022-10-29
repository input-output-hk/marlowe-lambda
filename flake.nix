{

  description = "Marlowe support for IOG's Contract-Lambda";

  inputs = {

    flake-utils.url = "github:numtide/flake-utils";

    haskell-nix.url = "github:input-output-hk/haskell.nix";

    nixpkgs.follows = "haskell-nix/nixpkgs-2205";

  };

  outputs = { self, nixpkgs, flake-utils, haskell-nix }:

    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:

      let

        overlays = [ haskell-nix.overlay
          (final: prev: {
            marlowe-lambda =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc8107";
                shell.tools = {
                  cabal                   = {};
                  ghcid                   = {};
                  haskell-language-server = {};
                  hie-bios                = {};
                  hlint                   = {};
                  pointfree               = {};
                };
                # Non-Haskell shell tools go here
                shell.buildInputs = with pkgs; [
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
        }

    );

}

{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    smart-primitives.url = "github:danielambda/smart-primitives";
  };

  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [inputs.haskell-flake.flakeModule];
      perSystem = { self', config, pkgs, ... }:
        let
        in {
        haskellProjects.default = {
          autoWire = ["packages"];
          packages = {
            smart-primitives.source = inputs.smart-primitives;
          };
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [config.haskellProjects.default.outputs.devShell];
          packages = [pkgs.nixd];

          shellHook = ''
            set -a
            source ./.env
            set +a
          '';
        };

        packages.default = self'.packages.check-check-backend-contracts;
      };
    };
}

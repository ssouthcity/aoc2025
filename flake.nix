{
  description = "Advent of Code 2025";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
    ...
  }: let
    systems = ["x86_64-linux"];
    eachSystem = nixpkgs.lib.genAttrs systems;
  in {
    packages = eachSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        default = pkgs.haskellPackages.callCabal2nix "aoc2025" ./. {};
      }
    );

    apps = eachSystem (system: let
      drv = self.packages.${system}.default;
      days = nixpkgs.lib.lists.range 1 12;
      formatDay = n: nixpkgs.lib.strings.fixedWidthString 2 "0" (toString n);
    in
      nixpkgs.lib.listToAttrs (map (n: let
        day = formatDay n;
      in {
        name = "day${day}";
        value = {
          type = "app";
          program = "${drv}/bin/day${day}";
          description = "Solution for day ${day}";
        };
      }) days));

    devShells = eachSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      default = pkgs.mkShell {
        buildInputs = [
          pkgs.haskellPackages.ghc
          pkgs.haskellPackages.cabal-install
        ];
      };
    });
  };
}

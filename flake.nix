{
  description = "My personal website";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/release-22.05;
    utils.url = github:numtide/flake-utils;
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import ./overlay.nix) ];
        pkgs = import nixpkgs { inherit overlays system; };
      in
      rec
      {
        packages.website = pkgs.website;
        packages.generator = pkgs.haskellPackages.generator;
        packages.default = packages.website;

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ p.generator ];
          buildInputs = with pkgs.haskellPackages; [
            cabal-install
            cabal-fmt
            haskell-language-server
            hlint
            fourmolu
          ];
          withHoogle = true;
          doBenchmark = false;
        };

        formatter = pkgs.nixpkgs-fmt;
      }
    );
}

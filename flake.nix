{
  description = ''Some1LikeYou - Sandy
    Typesafe Servant API with singletons.

    https://www.youtube.com/watch?v=PNkoUv74JQU
    https://reasonablypolymorphic.com/some1-like-you/#/step-34
    '';

  inputs = {
    # Nix Inputs
    nixpkgs.url = github:nixos/nixpkgs/24.05;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self , nixpkgs , flake-utils ,}:
    let
      utils = flake-utils.lib;
    in
    utils.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      hsPkgs = pkgs.haskellPackages.override {
        overrides = hfinal: hprev: {
          some1likeyou = hfinal.callCabal2nix "some1likeyou" ./. { };
        };
      };
    in
    rec {
      packages =
        utils.flattenTree
          { servant-example = hsPkgs.some1likeyou; };

      devShell = hsPkgs.shellFor {
        packages = p: [
          p.some1likeyou
        ];
        buildInputs = with pkgs;
          [
            pkgs.haskell-language-server
            haskellPackages.cabal-install
            haskellPackages.fourmolu
            haskellPackages.cabal-fmt
            nixpkgs-fmt
            zlib
          ];
      };

      defaultPackage = packages.some1likeyou;
    });
}

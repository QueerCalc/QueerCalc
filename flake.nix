{
  description = "graphing calculator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, crane, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        craneLib = crane.mkLib pkgs;

        commonArgs = {
          src = craneLib.cleanCargoSource ./.;
          strictDeps = true;

          buildInputs = with pkgs; [
            pkg-config
          ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
            fontconfig
            wayland-scanner.out
            xorg.libX11
            xorg.libXcursor
            xorg.libXrandr
            xorg.libXi
            freeglut
            libGL
            libxkbcommon
          ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
            pkgs.libiconv
          ];
        };

        queercalc = craneLib.buildPackage (commonArgs // {
          cargoArtifacts = craneLib.buildDepsOnly commonArgs;
          LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath commonArgs.buildInputs}";
        });
      in
      {
        checks = {
          inherit queercalc;
        };

        packages.default = queercalc;

        apps.default = flake-utils.lib.mkApp {
          drv = queercalc;
        };

        devShells.default = craneLib.devShell {
          checks = self.checks.${system};
          packages = [];
          LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath commonArgs.buildInputs}";
        };
      });
}

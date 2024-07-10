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
        inherit (pkgs) lib;

        craneLib = crane.mkLib pkgs;

        queercalc = craneLib.buildPackage {
          pname = "queercalc";
          version = "0.1.0";

          src = craneLib.cleanCargoSource ./.;
          strictDeps = true;

          buildInputs = with pkgs; [
            pkg-config
          ] ++ lib.optionals pkgs.stdenv.isLinux [
            fontconfig
            wayland-scanner.out
            xorg.libX11
            xorg.libXcursor
            xorg.libXrandr
            xorg.libXi
            freeglut
            libGL
            libxkbcommon
          ] ++ lib.optionals pkgs.stdenv.isDarwin [
            libiconv
          ];
        };
      in
      {
        checks = { inherit queercalc; };

        packages.default = queercalc;

        apps.default = flake-utils.lib.mkApp {
          drv = queercalc;
        };

        devShells.default = craneLib.devShell {
          checks = self.checks.${system};
          packages = queercalc.buildInputs;
          LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath queercalc.buildInputs}";
        };
      });
}

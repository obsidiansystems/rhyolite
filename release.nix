let
  lib = (import ./. {}).nixpkgs.lib;

  systems = [ "x86_64-linux" "x86_64-darwin" ];

in

  lib.genAttrs systems (system: let
    rhyolite = import ./. { inherit system; };
    reflex-platform = rhyolite.reflex-platform;
  in {

      # Used to build this project. Should only be needed by CI, devs.
      proj = reflex-platform.project ({ pkgs, ... }@args: {
        overrides = rhyolite.haskellOverrides;
        packages = {
          rhyolite-common = ./common;
          rhyolite-aeson-orphans = ./aeson-orphans;
          rhyolite-backend = ./backend;
          rhyolite-backend-db = ./backend-db;
          rhyolite-backend-db-gargoyle = ./backend-db-gargoyle;
          rhyolite-backend-snap = ./backend-snap;
          rhyolite-datastructures = ./datastructures;
          rhyolite-frontend = ./frontend;
        };
        shells = rec {
          ghc = [
            "rhyolite-backend"
            "rhyolite-backend-db"
            "rhyolite-backend-db-gargoyle"
            "rhyolite-backend-snap"
          ] ++ ghcjs;
          ghcjs = [
            "rhyolite-aeson-orphans"
            "rhyolite-common"
            "rhyolite-datastructures"
            "rhyolite-frontend"
          ];
        };
        tools = ghc: [ pkgs.postgresql ];
      });

  })

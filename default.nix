{ obelisk ? import ../obelisk (builtins.removeAttrs args ["pkgs" "inNixShell"])
, pkgs ? obelisk.nixpkgs
, ... } @ args:

let
  reflex-platform = obelisk.reflex-platform;
  inherit (pkgs) lib;
  haskellLib = pkgs.haskell.lib;
  repos = pkgs.thunkSet ./dep;

  # Some dependency thunks needed
  dep = import ./dep reflex-platform.hackGet;
  #TODO: Consider whether to prefer using thunkSet here.

  # Local packages. We override them below so that other packages can use them.
  rhyolitePackages = {
    rhyolite-backend = ./backend;
    rhyolite-beam-db = ./beam/db;
    rhyolite-beam-orphans = ./beam/orphans;
    rhyolite-beam-task-worker-types = ./beam/task/types;
    rhyolite-beam-task-worker-backend = ./beam/task/backend;
    rhyolite-notify-listen = ./notify-listen/notify-listen;
    rhyolite-notify-listen-beam = ./notify-listen/notify-listen-beam;
    psql-simple-class = ./psql-extras/psql-simple-class;
    psql-simple-beam = ./psql-extras/psql-simple-beam;
    psql-simple-groundhog = ./psql-extras/psql-simple-groundhog;
    psql-serializable = ./psql-extras/psql-serializable;
    rhyolite-groundhog-legacy = ./groundhog-legacy/groundhog-legacy;
    rhyolite-groundhog-legacy-types = ./groundhog-legacy/groundhog-legacy-types;
    rhyolite-common = ./common;
    rhyolite-email = ./email;
    mime-mail-orphans = ./email/mime-mail-orphans;
    semimap = ./semimap;
    rhyolite-frontend = ./frontend;
    signed-data = ./signed-data/signed-data;
    signed-data-clientsession = ./signed-data/signed-data-clientsession;
    rhyolite-widgets = ./widgets;
    rhyolite-account-backend = ./account/backend;
    rhyolite-account-types = ./account/types;
  };

  genOverlays = attr: builtins.map (x: { name = x; version = "0.1"; src = attr."${x}";  }) (builtins.attrNames attr);
  rhyoliteOverlays = genOverlays rhyolitePackages;
in {
  inherit rhyolitePackages;
  inherit rhyoliteOverlays;
  proj = obelisk.project {} ({ ... }: {
    name = "skeleton";
    src = ./.;
    shells = ps: with ps; [
      backend
    ];
    overrides = [
      ({pkgs, lib, config, ... }: {
          packages.obelisk-run.components.library.build-tools = with pkgs; [
            iproute
          ];
      })
    ];
  });
}

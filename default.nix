{ obelisk ? import ./dep/obelisk (builtins.removeAttrs args ["pkgs" "inNixShell"])
, pkgs ? obelisk.pkgs
, ... } @ args:

let
  nix-thunk = import ./dep/nix-thunk { inherit pkgs; };

  # Local packages. We override them below so that other packages can use them.
  rhyolitePackages = {
    iv-backend = ./iv/backend;
    iv-common = ./iv/common;
    obelisk-aeson-orphans = ./aeson-orphans;
    rhyolite-backend = ./backend;
    rhyolite-beam-db = ./beam/db;
    rhyolite-beam-orphans = ./beam/orphans;
    rhyolite-beam-task-worker-types = ./beam/task/types;
    rhyolite-beam-task-worker-backend = ./beam/task/backend;
    rhyolite-notify-listen = ./notify-listen/notify-listen;
    rhyolite-notify-listen-beam = ./notify-listen/notify-listen-beam;
    psql-simple-class = ./psql-extras/psql-simple-class;
    psql-simple-beam = ./psql-extras/psql-simple-beam;
    psql-serializable = ./psql-extras/psql-serializable;
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

  repos = nix-thunk.mapSubdirectories
    nix-thunk.thunkSource
    ./dep;

  # srcs used for overrides
  haskellPackageSources = rhyolitePackages // {
    beam-sqlite = repos.beam + "/beam-sqlite";
    beam-core = repos.beam + "/beam-core";
    beam-postgres = repos.beam + "/beam-postgres";
    beam-migrate = repos.beam + "/beam-migrate";
    # This is not needed and doesn't build
    # beam-migrate-cli = repos.beam + "/beam-migrate-cli";

    beam-automigrate = repos.beam-automigrate;

    beam-transformers-backend = repos.beam-transformers + "/backend";
    beam-transformers-common = repos.beam-transformers + "/common";

    bytestring-aeson-orphans = repos.bytestring-aeson-orphans;
    bytestring-trie = repos.bytestring-trie;
    monoid-map = repos.monoid-map;
    postgresql-simple = repos.postgresql-simple;
    postgresql-simple-interpolate = repos.postgresql-simple-interpolate;

    # Newer versions than those in Reflex Platform
    gargoyle = repos.gargoyle + "/gargoyle";
    gargoyle-postgresql = repos.gargoyle + "/gargoyle-postgresql";
    gargoyle-postgresql-connect = repos.gargoyle + "/gargoyle-postgresql-connect";
    gargoyle-nix-postgres-monitor = repos.gargoyle + "/gargoyle-nix-postgres-monitor";
    gargoyle-postgresql-nix = repos.gargoyle + "/gargoyle-postgresql-nix";

    #TODO: Fix
    # push-notifications = repos.push-notifications;

    vessel = repos.vessel;
  };

in obelisk // {

  inherit rhyolitePackages haskellPackageSources;

  # Function similar to obelisk.project that handles overrides for you.
  project = base: projectDefinition:
    obelisk.project base ({...}@args:
      let def = projectDefinition args;
      in def // {
        inputThunks = haskellPackageSources // def.inputThunks or {};
      });

  # Used to build this project. Should only be needed by CI, devs.
  proj = obelisk.project ({ pkgs, ... }@args: {
    inputThunks = haskellPackageSources;
    shells = rec {
      ghc = builtins.attrNames rhyolitePackages;
      ghcjs = [
        "rhyolite-common"
        "rhyolite-frontend"
      ];
    };
    tools = ghc: [ pkgs.postgresql ];
  });
}

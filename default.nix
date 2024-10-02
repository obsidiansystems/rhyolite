{ obelisk ? import ./dep/obelisk (builtins.removeAttrs args ["pkgs" "inNixShell"])
, pkgs ? obelisk.nixpkgs
, ... } @ args:

let
  reflex-platform = obelisk.reflex-platform;
  inherit (pkgs) lib;
  haskellLib = pkgs.haskell.lib;
  repos = pkgs.mapSubdirectories pkgs.thunkSource ./dep;

  # Some dependency thunks needed
  dep = import ./dep reflex-platform.hackGet;
  #TODO: Consider whether to prefer using thunkSet here.

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

  # srcs used for overrides
  overrideSrcs = rhyolitePackages // {
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
    monoid-map = repos.monoid-map;
    postgresql-simple = repos.postgresql-simple;
    postgresql-simple-interpolate = repos.postgresql-simple-interpolate;

    # Newer versions than those in reflex-platform
    gargoyle = repos.gargoyle + "/gargoyle";
    gargoyle-postgresql = repos.gargoyle + "/gargoyle-postgresql";
    gargoyle-postgresql-connect = repos.gargoyle + "/gargoyle-postgresql-connect";
    gargoyle-nix-postgres-monitor = repos.gargoyle + "/gargoyle-nix-postgres-monitor";
    gargoyle-postgresql-nix = repos.gargoyle + "/gargoyle-postgresql-nix";
    vessel = repos.vessel;
    postgresql-lo-stream = repos.postgresql-lo-stream;

    #TODO: Fix
    # push-notifications = repos.push-notifications;

  };

  # You can use these manually if you don’t want to use rhyolite.project.
  # It will be needed if you need to combine with multiple overrides.
  haskellOverrides = lib.foldr lib.composeExtensions (_: _: {}) [
    (self: super: lib.mapAttrs (name: path: self.callCabal2nix name path {}) overrideSrcs)
    (self: super: {
      frontend = super.frontend.override {
        obelisk-executable-config-lookup = self.obelisk-executable-config-lookup;
      };
      beam-postgres = haskellLib.dontCheck super.beam-postgres;
      beam-migrate = haskellLib.dontCheck super.beam-migrate;
      gargoyle-postgresql-nix = haskellLib.overrideCabal super.gargoyle-postgresql-nix {
        librarySystemDepends = [ pkgs.postgresql ];
      };
      postgresql-simple = haskellLib.dontCheck super.postgresql-simple;
      validation = haskellLib.dontCheck super.validation;
      postgresql-lo-stream = haskellLib.markUnbroken super.postgresql-lo-stream;

      HaskellNet-SSL = self.callHackage "HaskellNet-SSL" "0.3.4.4" {};

      base-orphans = self.callHackageDirect {
        pkg = "base-orphans";
        ver = "0.8.6";
        sha256 = "sha256:17hplm1mgw65jbszg5z4vqk4i24ilxv8mbszr3s8lhpll5naik26";
      } {};

      aeson-qq = self.callHackage "aeson-qq" "0.8.4" {};
      postgresql-syntax = haskellLib.dontCheck super.postgresql-syntax;
      vessel = haskellLib.doJailbreak super.vessel;
      monoid-map = haskellLib.doJailbreak super.monoid-map;

      # 'locale' is broken on nix darwin which is required by postgres 'initdb'
      rhyolite-beam-task-worker-backend = if pkgs.stdenv.hostPlatform.isDarwin
      then
        haskellLib.dontCheck super.rhyolite-beam-task-worker-backend
      else
        super.rhyolite-beam-task-worker-backend;
    })
  ];

in obelisk // {

  inherit haskellOverrides;

  rhyolitePackages = haskellPackages: builtins.intersectAttrs rhyolitePackages (haskellPackages.extend haskellOverrides);

  haskellPackageSources = overrideSrcs;

  # Function similar to obelisk.project that handles overrides for you.
  project = base: projectDefinition:
    obelisk.project base ({...}@args:
      let def = projectDefinition args;
      in def // {
        overrides = lib.composeExtensions haskellOverrides (def.overrides or (_: _: {}));
      });

  # Used to build this project. Should only be needed by CI, devs.
  proj = obelisk.reflex-platform.project ({ pkgs, ... }@args: {
    overrides = haskellOverrides;
    packages = rhyolitePackages;
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

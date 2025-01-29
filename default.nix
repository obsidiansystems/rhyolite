{ obelisk ? import ./dep/obelisk (builtins.removeAttrs args ["pkgs" "inNixShell"])
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
    bytestring-aeson-orphans = repos.bytestring-aeson-orphans;
    monoid-map = repos.monoid-map;
    postgresql-simple-interpolate = repos.postgresql-simple-interpolate;

    # Newer versions than those in reflex-platform
    gargoyle = repos.gargoyle + "/gargoyle";
    gargoyle-postgresql = repos.gargoyle + "/gargoyle-postgresql";
    gargoyle-postgresql-connect = repos.gargoyle + "/gargoyle-postgresql-connect";
    gargoyle-postgresql-nix = repos.gargoyle + "/gargoyle-postgresql-nix";
    push-notifications = repos.push-notifications;
    vessel = repos.vessel;
    postgresql-lo-stream = repos.postgresql-lo-stream;
    beam-automigrate = repos.beam-automigrate;

  };

  # You can use these manually if you don’t want to use rhyolite.project.
  # It will be needed if you need to combine with multiple overrides.
  haskellOverrides = lib.foldr lib.composeExtensions (_: _: {}) [
    (self: super: lib.mapAttrs (name: path: self.callCabal2nix name path {}) overrideSrcs)
    (self: super: {
      frontend = super.frontend.override {
        obelisk-executable-config-lookup = self.obelisk-executable-config-lookup;
      };
      gargoyle-postgresql-nix = haskellLib.overrideCabal super.gargoyle-postgresql-nix {
        librarySystemDepends = [ pkgs.postgresql ];
      };
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

      beam-migrate = self.callHackage "beam-migrate" "0.5.2.0" {};

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
    packages = {
      rhyolite-backend = ./backend;
      rhyolite-common = ./common;
      semimap = ./semimap;
      rhyolite-frontend = ./frontend;
      rhyolite-test-suite = ./test;
    };
    shells = rec {
      ghc = [
        "rhyolite-backend"
        "rhyolite-test-suite"
      ] ++ ghcjs;
      ghcjs = [
        "rhyolite-common"
        "rhyolite-frontend"
      ];
    };
    tools = ghc: [ pkgs.postgresql ];
  });
}

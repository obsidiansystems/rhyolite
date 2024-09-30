{ obelisk ? import ./dep/obelisk (builtins.removeAttrs args ["pkgs" "inNixShell"] // { useGHC810 = true; })
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
    rhyolite-test-suite = ./test;
  };

  # srcs used for overrides
  overrideSrcs = rhyolitePackages // {
    beam-sqlite = repos.beam + "/beam-sqlite";
    beam-core = repos.beam + "/beam-core";
    beam-postgres = repos.beam + "/beam-postgres";
    beam-migrate = repos.beam + "/beam-migrate";
    beam-migrate-cli = repos.beam + "/beam-migrate-cli";

    beam-transformers-backend = repos.beam-transformers + "/backend";
    beam-transformers-common = repos.beam-transformers + "/common";

    bytestring-aeson-orphans = repos.bytestring-aeson-orphans;
    bytestring-trie = repos.bytestring-trie;
    monoid-map = repos.monoid-map;
    postgresql-simple = repos.postgresql-simple;
    postgresql-simple-interpolate = repos.postgresql-simple-interpolate;

    # Newer versions than those in reflex-platform
    gargoyle = repos.gargoyle + "/gargoyle";
    gargoyle-postgresql = repos.gargoyle + "/gargoyle-postgresql";
    gargoyle-postgresql-connect = repos.gargoyle + "/gargoyle-postgresql-connect";
    gargoyle-postgresql-nix = repos.gargoyle + "/gargoyle-postgresql-nix";
    push-notifications = repos.push-notifications;

  };

  # You can use these manually if you don’t want to use rhyolite.project.
  # It will be needed if you need to combine with multiple overrides.
  haskellOverrides = lib.foldr lib.composeExtensions (_: _: {}) [
    (self: super: lib.mapAttrs (name: path: self.callCabal2nix name path {}) overrideSrcs)
    (self: super: {
      frontend = super.frontend.override {
        obelisk-executable-config-lookup = self.obelisk-executable-config-lookup;
      };
      beam-automigrate = haskellLib.doJailbreak super.beam-automigrate;
      beam-postgres = haskellLib.dontCheck super.beam-postgres;
      beam-migrate = haskellLib.dontCheck super.beam-migrate;
      bytestring-trie = haskellLib.dontCheck super.bytestring-trie;
      gargoyle-postgresql-nix = haskellLib.overrideCabal super.gargoyle-postgresql-nix {
        librarySystemDepends = [ pkgs.postgresql ];
      };
      postgresql-simple = haskellLib.dontCheck super.postgresql-simple;
      validation = haskellLib.dontCheck super.validation;

      postgresql-lo-stream = haskellLib.doJailbreak (self.callHackageDirect {
        pkg = "postgresql-lo-stream";
        ver = "0.1.1.1";
        sha256 = "0ifr6i6vygckj2nikv7k7yqia495gnn27pq6viasckmmh6zx6gwi";
      } {});

      monad-logger-extras = self.callHackageDirect {
        pkg = "monad-logger-extras";
        ver = "0.1.1.1";
        sha256 = "17dr2jwg1ig1gd4hw7160vf3l5jcx5p79b2lz7k17f6v4ygx3vbz";
      } {};
      monoid-subclasses = self.callHackageDirect {
        pkg = "monoid-subclasses";
        ver = "1.1";
        sha256 = "02ggjcwjdjh6cmy7zaji5mcmnq140sp33cg9rvwjgply6hkddrvb";
      } {};
      HaskellNet = self.callHackage "HaskellNet" "0.6" {};
      HaskellNet-SSL = self.callHackage "HaskellNet-SSL" "0.3.4.4" {};

      base-orphans = self.callHackageDirect {
        pkg = "base-orphans";
        ver = "0.8.6";
        sha256 = "sha256:17hplm1mgw65jbszg5z4vqk4i24ilxv8mbszr3s8lhpll5naik26";
      } {};

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

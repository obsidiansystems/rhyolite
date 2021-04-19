{ obelisk ? import ./.obelisk/impl (builtins.removeAttrs args ["pkgs"])
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
    rhyolite-aeson-orphans = ./aeson-orphans;
    rhyolite-backend = ./backend;
    rhyolite-backend-db = ./backend-db;
    rhyolite-backend-snap = ./backend-snap;
    rhyolite-common = ./common;
    rhyolite-datastructures = ./datastructures;
    rhyolite-frontend = ./frontend;
  };

  # srcs used for overrides
  overrideSrcs = rhyolitePackages // {
    bytestring-trie = repos.bytestring-trie;
    dependent-monoidal-map = repos.dependent-monoidal-map;
    groundhog = repos.groundhog + /groundhog;
    groundhog-postgresql = repos.groundhog + /groundhog-postgresql;
    groundhog-th = repos.groundhog + /groundhog-th;
    HaskellNet = repos.HaskellNet; # (super is marked as broken) unreleased fixes for newer GHC
    HaskellNet-SSL = repos.HaskellNet-SSL; # (super is marked as broken)
    postgresql-simple = repos.postgresql-simple;  # v0.5.4.0 with a fix

    # Newer versions than those in reflex-platform
    gargoyle = repos.gargoyle + /gargoyle;
    gargoyle-postgresql = repos.gargoyle + /gargoyle-postgresql;
    gargoyle-postgresql-connect = repos.gargoyle + /gargoyle-postgresql-connect;
    gargoyle-postgresql-nix = repos.gargoyle + /gargoyle-postgresql-nix;
    database-id-class = repos.database-id + "/class";
    database-id-groundhog = repos.database-id + "/groundhog";
    database-id-obelisk = repos.database-id + "/obelisk";
    push-notifications = repos.push-notifications;
    vessel = repos.vessel;
    dependent-sum-aeson-orphans = repos.dependent-sum-aeson-orphans;
  };

  # You can use these manually if you don’t want to use rhyolite.project.
  # It will be needed if you need to combine with multiple overrides.
  haskellOverrides = lib.foldr lib.composeExtensions (_: _: {}) [
    (self: super: lib.mapAttrs (name: path: self.callCabal2nix name path {}) overrideSrcs)
    (self: super: {
      bytestring-trie = haskellLib.dontCheck super.bytestring-trie;
      dependent-monoidal-map = haskellLib.doJailbreak super.dependent-monoidal-map;
      gargoyle-postgresql-nix = haskellLib.overrideCabal super.gargoyle-postgresql-nix { librarySystemDepends = [ pkgs.postgresql ]; };
      postgresql-simple = haskellLib.dontCheck (
          haskellLib.overrideCabal super.postgresql-simple {
            revision = null;
            editedCabalFile = null;
          }
        );
      validation = haskellLib.dontCheck super.validation;

      postgresql-lo-stream = self.callHackageDirect {
        pkg = "postgresql-lo-stream";
        ver = "0.1.1.1";
        sha256 = "0ifr6i6vygckj2nikv7k7yqia495gnn27pq6viasckmmh6zx6gwi";
      } {};

      monad-logger-extras = self.callHackageDirect {
        pkg = "monad-logger-extras";
        ver = "0.1.1.1";
        sha256 = "17dr2jwg1ig1gd4hw7160vf3l5jcx5p79b2lz7k17f6v4ygx3vbz";
      } {};
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
      rhyolite-aeson-orphans = ./aeson-orphans;
      rhyolite-backend = ./backend;
      rhyolite-backend-db = ./backend-db;
      rhyolite-backend-snap = ./backend-snap;
      rhyolite-common = ./common;
      rhyolite-datastructures = ./datastructures;
      rhyolite-frontend = ./frontend;
      rhyolite-test-suite = ./test;
    };
    shells = rec {
      ghc = [
        "rhyolite-backend"
        "rhyolite-backend-db"
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
}

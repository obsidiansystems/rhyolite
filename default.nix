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
    rhyolite-logging = ./logging;
  };

  # srcs used for overrides
  overrideSrcs = rhyolitePackages // {
    aeson-gadt-th = repos.aeson-gadt-th;
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
    #libsystemd-journal = repos.libsystemd-journal; # TODO: Remove override once nixpkgs is bumped to include 1.4.4.
    # Newly added to hackage
    database-id-class = repos.database-id + /class;
    database-id-groundhog = repos.database-id + /groundhog;
    database-id-obelisk = repos.database-id + /obelisk;
    push-notifications = repos.push-notifications;
    vessel = repos.vessel;
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

      dependent-sum-aeson-orphans = self.callHackageDirect {
        pkg = "dependent-sum-aeson-orphans";
        ver = "0.2.1.0";
        sha256 = "18ahi78f42dlh1gq0lzab0ik8c3vvlphdic4abywcqv2q0ilqxq7";
      } {};

      postgresql-lo-stream = self.callHackageDirect {
        pkg = "postgresql-lo-stream";
        ver = "0.1.1.1";
        sha256 = "0ifr6i6vygckj2nikv7k7yqia495gnn27pq6viasckmmh6zx6gwi";
      } {};

      # TODO: Remove override once nixpkgs is bumped to include 1.4.4.
      libsystemd-journal = haskellLib.overrideCabal (
        self.callHackageDirect {
          pkg = "libsystemd-journal";
          ver = "1.4.4";
          sha256 = "0cdvyqf2dg7d1ccgpk8fhiq7gjkh9rzcl8zy8fwq2d87lmxxn1dr";
        } {}
      ) { libraryPkgconfigDepends = [ pkgs.systemd ]; };
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
      rhyolite-logging = ./rhyolite-logging;
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

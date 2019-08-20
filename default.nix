{ pkgs ? null, ... } @ args:

let

  obelisk = import ./.obelisk/impl (builtins.removeAttrs args ["pkgs"]);
  reflex-platform = obelisk.reflex-platform;
  nixpkgs = if pkgs == null then obelisk.nixpkgs else pkgs;
  inherit (nixpkgs) lib;
  haskellLib = nixpkgs.haskell.lib;
  repos = obelisk.nixpkgs.thunkSet ./dep;

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
    aeson-gadt-th = repos.aeson-gadt-th;
    bytestring-trie = repos.bytestring-trie;
    dependent-sum-aeson-orphans = repos.dependent-sum-aeson-orphans;
    groundhog = repos.groundhog + /groundhog;
    groundhog-postgresql = repos.groundhog + /groundhog-postgresql;
    groundhog-th = repos.groundhog + /groundhog-th;
    monoidal-containers = repos.monoidal-containers;
    postgresql-lo-stream = repos.postgresql-lo-stream;
    # Newer versions than those in reflex-platform
    gargoyle = repos.gargoyle + /gargoyle;
    gargoyle-postgresql = repos.gargoyle + /gargoyle-postgresql;
    gargoyle-postgresql-connect = repos.gargoyle + /gargoyle-postgresql-connect;
    gargoyle-postgresql-nix = repos.gargoyle + /gargoyle-postgresql-nix;
    # Newly added to hackage
    database-id-class = repos.database-id + /class;
    database-id-groundhog = repos.database-id + /groundhog;
    database-id-obelisk = repos.database-id + /obelisk;
    push-notifications = repos.push-notifications;
    reflex = repos.reflex;
  };

  # You can use these manually if you don’t want to use rhyolite.project.
  # It will be needed if you need to combine with multiple overrides.
  haskellOverrides = lib.foldr lib.composeExtensions  (_: _: {}) [
    (self: super: lib.mapAttrs (name: path: self.callCabal2nix name path {}) overrideSrcs)
    (self: super: {
      bytestring-trie = haskellLib.dontCheck super.bytestring-trie;
      gargoyle-postgresql-nix = haskellLib.overrideCabal super.gargoyle-postgresql-nix { librarySystemDepends = [ nixpkgs.postgresql ]; };
      validation = haskellLib.dontCheck super.validation;
    })
  ];

in obelisk // {

  inherit haskellOverrides;

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

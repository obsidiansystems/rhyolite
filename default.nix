{ obelisk ? import ./.obelisk/impl {}
, pkgs ? obelisk.nixpkgs, ... } @ args:

let

  reflex-platform = obelisk.reflex-platform;
  inherit (pkgs) lib;
  haskellLib = pkgs.haskell.lib;

  # Some dependency thunks needed
  repos = {

    # Not sure why this is needed?
    groundhog = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "groundhog";
      rev = "febd6c12a676693b1d7339e54a4d107c4a67fcc3";
      sha256 = "1q05nrqdzh26r17wsd53sdj106dxh3qlg66pqr3jsi8d63iyaq8k";
    };

    # bytestring-trie in hackage doesn’t support base 4.11+
    bytestring-trie = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "bytestring-trie";
      rev = "27117ef4f9f01f70904f6e8007d33785c4fe300b";
      sha256 = "103fqr710pddys3bqz4d17skgqmwiwrjksn2lbnc3w7s01kal98a";
    };

    # Rhyolite needs throttleBatchWithLag from Reflex.Time
    reflex = pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex";
      rev = "2e2e19d37a5f1151fe985190b479016640751578";
      sha256 = "0ygp3firh2h770wsfpzykph8jlb3xpi3bdl0imqa6zgcbphf13vx";
    };

    # Reflex needs monoidal-containers 0.4.0.0 but that is not
    # available to some older Obelisk versions.
    monoidal-containers = pkgs.fetchFromGitHub {
      owner = "bgamari";
      repo = "monoidal-containers";
      rev = "a34c9fbe191725ef9a9c7783e103c24796bd91e3";
      sha256 = "1ar2w4rx0mh4nvwzpc125l3hj9xslargl43vnssmh9l6ynhi8ksv";
    };

    # New version, recently added to hackage
    constraints-extras = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "constraints-extras";
      rev = "4ef826429ddcf0e110fdee06c9683642d88feb6b";
      sha256 = "11dsvf9vdaj12jdjg1na17b1347vaph2qyywiv8rq4r19rad5h88";
    };

    # Newly added to hackage
    aeson-gadt-th = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "aeson-gadt-th";
      rev = "5aabb547893b8a1140ddad4fba5a266db1d08fbe";
      sha256 = "0sywn3w7ph7b7r0byjz7c9cy41hb8p3ym8qljg1bd99d92hj3sig";
    };

    # Newly added to hackage
    postgresql-lo-stream = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "postgresql-lo-stream";
      rev = "33e1a64c1f65d7d1e26d6d08d2ddb85eb795f94c";
      sha256 = "0n2cmmplljq3z3n0piyiq4vvx8d48byi5isr520aq6dv35j5ixim";
    };
  };

  # Local packages. We override them below so that other packages can use them.
  rhyolitePackages = {
    rhyolite-common = ./common;
    rhyolite-aeson-orphans = ./aeson-orphans;
    rhyolite-backend = ./backend;
    rhyolite-backend-db = ./backend-db;
    rhyolite-backend-db-gargoyle = ./backend-db-gargoyle;
    rhyolite-backend-snap = ./backend-snap;
    rhyolite-datastructures = ./datastructures;
    rhyolite-frontend = ./frontend;
  };

  # srcs used for overrides
  overrideSrcs = rhyolitePackages // {
    groundhog = repos.groundhog + /groundhog;
    groundhog-postgresql = repos.groundhog + /groundhog-postgresql;
    groundhog-th = repos.groundhog + /groundhog-th;
    bytestring-trie = repos.bytestring-trie;
    reflex = repos.reflex;
    monoidal-containers = repos.monoidal-containers;
    constraints-extras = repos.constraints-extras;
    aeson-gadt-th = repos.aeson-gadt-th;
    postgresql-lo-stream = repos.postgresql-lo-stream;
  };

  # You can use these manually if you don’t want to use rhyolite.project.
  # It will be needed if you need to combine with multiple overrides.
  haskellOverrides = lib.foldr lib.composeExtensions  (_: _: {}) [
    (self: super: lib.mapAttrs (name: path: self.callCabal2nix name path {}) overrideSrcs)
    (self: super: {
      bytestring-trie = haskellLib.dontCheck super.bytestring-trie;
      reflex = haskellLib.dontCheck super.reflex;
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

}

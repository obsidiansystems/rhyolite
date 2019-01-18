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
      rev = "be91bc55eb7cc3c3036f12af75a45012b08913ec";
      sha256 = "105wdpbdbadb0kncjzax6islrndfzv60qfxzsrwb3490z692mha7";
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

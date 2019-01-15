{ ... } @ args:

let

  obelisk = import ./.obelisk/impl args;
  pkgs = obelisk.nixpkgs;
  inherit (pkgs) lib;
  haskellLib = pkgs.haskell.lib;

  # Some dependency thunks needed
  repos = {

    # Not sure why this is needed?
    gargoyle = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "gargoyle";
      rev = "2c19c569325ad76694526e9b688ccdbf148df980";
      sha256 = "0257p0qd8xx900ngghkjbmjnvn7pjv05g0jm5kkrm4p6alrlhfyl";
    };

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
  };

  # Local packages
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
    gargoyle = repos.gargoyle + /gargoyle;
    gargoyle-nix = repos.gargoyle + /gargoyle-nix;
    gargoyle-postgresql = repos.gargoyle + /gargoyle-postgresql;
    gargoyle-postgresql-nix = repos.gargoyle + /gargoyle-postgresql-nix;
  };

  haskellOverrides = lib.composeExtensions
    (self: super: lib.mapAttrs (name: path: self.callCabal2nix name path {}) overrideSrcs)
    (self: super: {
      bytestring-trie = haskellLib.dontCheck super.bytestring-trie;
      reflex = haskellLib.dontCheck super.reflex;

      gargoyle = haskellLib.doJailbreak super.gargoyle;
      gargoyle-nix = haskellLib.doJailbreak super.gargoyle-nix;
      gargoyle-postgresql = haskellLib.doJailbreak super.gargoyle-postgresql;
      gargoyle-postgresql-nix = haskellLib.doJailbreak (haskellLib.addBuildTools
        super.gargoyle-postgresql-nix [ pkgs.postgresql ]);
    });


  # Function similar to obelisk.project that handles overrides for you.
  project = base: projectDefinition:
    obelisk.project base ({...}@args:
      let def = projectDefinition args;
      in def // {
        overrides = lib.composeExtensions haskellOverrides (def.overrides or (_: _: {}));
      });

in obelisk // {

  # Similar to obelisk.project
  inherit project;

  # Use these manually if you don’t want to use rhyolite.project.
  inherit haskellOverrides;

  # Used to build this project. Should only be needed by CI, devs.
  proj = project ./. (_: {
    packages = rhyolitePackages;
  });

  # ALIAS for tezos-bake-central.
  lib = _: { inherit haskellOverrides; };

}

{ pkgs }:

let

  inherit (pkgs) lib;
  haskellLib = pkgs.haskell.lib;

  # Some dependency thunks needed
  repos = {

    # Not sure why this is needed?
    gargoyle = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "gargoyle";
      rev = "b641902ff1e798e230e5a101fd22ebfbae3c6a08";
      sha256 = "0pxa2zn1vy4n9cpal1cy1lcpmzgjvrx0ppmjqjs7ykv7z8647f1g";
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
    gargoyle = repos.gargoyle + /gargoyle;
    gargoyle-postgresql = repos.gargoyle + /gargoyle-postgresql;
    gargoyle-postgresql-nix = repos.gargoyle + /gargoyle-postgresql-nix;
  };

in lib.foldr lib.composeExtensions  (_: _: {}) [
  (self: super: lib.mapAttrs (name: path: self.callCabal2nix name path {}) overrideSrcs)
  (self: super: {
    bytestring-trie = haskellLib.dontCheck super.bytestring-trie;
    reflex = haskellLib.dontCheck super.reflex;
    gargoyle-postgresql-nix = haskellLib.addBuildTools super.gargoyle-postgresql-nix
      [ pkgs.postgresql ]; # TH use of `staticWhich` for `psql` requires this on the PATH during build time.);
  })
]

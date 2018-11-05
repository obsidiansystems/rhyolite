let
  lib = { pkgs, ... }: pkgs.lib.makeExtensible (libSelf: {
    repos = {
      gargoyle = pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "gargoyle";
        rev = "2c19c569325ad76694526e9b688ccdbf148df980";
        sha256 = "0257p0qd8xx900ngghkjbmjnvn7pjv05g0jm5kkrm4p6alrlhfyl";
      };
      groundhog = pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "groundhog";
        rev = "febd6c12a676693b1d7339e54a4d107c4a67fcc3";
        sha256 = "1q05nrqdzh26r17wsd53sdj106dxh3qlg66pqr3jsi8d63iyaq8k";
      };
      bytestring-trie = pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "bytestring-trie";
        rev = "27117ef4f9f01f70904f6e8007d33785c4fe300b";
        sha256 = "103fqr710pddys3bqz4d17skgqmwiwrjksn2lbnc3w7s01kal98a";
      };
    };

    srcs = {
      constraints-extras = pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "constraints-extras";
        rev = "abd1bab0738463657fc6303e606015a97b01c8a0";
        sha256 = "0lpc3cy8a7h62zgqf214g5bf68dg8clwgh1fs8hada5af4ppxf0l";
      };

      # gargoyle = libSelf.repos.gargoyle + /gargoyle;
      # gargoyle-postgresql = libSelf.repos.gargoyle + /gargoyle-postgresql;

      groundhog = libSelf.repos.groundhog + /groundhog;
      groundhog-postgresql = libSelf.repos.groundhog + /groundhog-postgresql;
      groundhog-th = libSelf.repos.groundhog + /groundhog-th;

      rhyolite-aeson-orphans = ./aeson-orphans;
      rhyolite-backend = ./backend;
      rhyolite-backend-db = ./backend-db;
      rhyolite-backend-db-gargoyle = ./backend-db-gargoyle;
      rhyolite-backend-snap = ./backend-snap;
      rhyolite-datastructures = ./datastructures;
      rhyolite-frontend = ./frontend;

      websockets = pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "websockets";
        rev = "1493961d12c30c786b568df09d285582bc649fbc";
        sha256 = "17gf1xpj57gskigczxl7pk6n5iz6lbq3p8395755v1kfl37cdb5a";
      };
    };

    haskellOverrides = pkgs.lib.composeExtensions
      (self: super: pkgs.lib.mapAttrs (name: path: self.callCabal2nix name path {}) libSelf.srcs)
      (self: super: {
        bytestring-trie = pkgs.haskell.lib.dontCheck (self.callCabal2nix "bytestring-trie" libSelf.repos.bytestring-trie {});
        gargoyle = pkgs.haskell.lib.doJailbreak             (self.callCabal2nix "gargoyle" (libSelf.repos.gargoyle + /gargoyle) {});
        gargoyle-nix = pkgs.haskell.lib.doJailbreak         (self.callCabal2nix "gargoyle-nix" (libSelf.repos.gargoyle + /gargoyle-nix) {});
        gargoyle-postgresql = pkgs.haskell.lib.doJailbreak  (self.callCabal2nix "gargoyle-postgresql" (libSelf.repos.gargoyle + /gargoyle-postgresql) {});
        gargoyle-postgresql-nix = pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.addBuildTools
          (self.callCabal2nix "gargoyle-postgresql-nix" (libSelf.repos.gargoyle + /gargoyle-postgresql-nix) {})
          [ pkgs.postgresql ]); # TH use of `staticWhich` for `psql` requires this on the PATH during build time.
        heist = pkgs.haskell.lib.doJailbreak super.heist;
        monad-logger = if (self.ghc.isGhcjs or false) then null else super.monad-logger;
        pipes-binary = pkgs.haskell.lib.doJailbreak super.pipes-binary;
        reflex = pkgs.haskell.lib.dontCheck (self.callPackage (pkgs.fetchFromGitHub {
          owner = "reflex-frp";
          repo = "reflex";
          rev = "3614d95a4059d4e202374eaa47a24cfabe9342b1"; # branch jd-fancy-throttle+eventwritert-adjustable-fix
          sha256 = "00w31siv4xlpx887vnl5n6bw8bhy50qr52x30mw3hb1lcqqpb4vh";
        }) {});
        rhyolite-common = self.callPackage ./common {};
      });
  });

  proj = { pkgs ? import <nixpkgs> {} }:
    let
      obeliskImpl = pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "obelisk";
        rev = "ede7d566b7e9362e2bf55a9ea291f2ca2a3ea9d4";
        sha256 = "07l9acq0hyr6af7jc48s5fzvfbvn6m81h1ln3ha5wvrgzmx3dbsh";
      };
      reflex-platform = (import obeliskImpl {}).reflex-platform;
    in reflex-platform.project ({ pkgs, ... }@args: {
      packages = {
        # In an obelisk project, these will be added by `obelisk.project`.
        # Since this is not *actually* an obelisk project, we need to supply these manually.
        obelisk-asset-serve-snap = obeliskImpl + /lib/asset/serve-snap;
        obelisk-snap-extras = obeliskImpl + /lib/snap-extras;
      };
      overrides = (lib args).haskellOverrides;
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
in {
  inherit proj lib;
}

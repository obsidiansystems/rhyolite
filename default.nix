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
        rev = "c2f18be45e3233f6268c8468eb0732dd6b2e8009";
        sha256 = "1r9i78bsnm6idbvp87gjklnr10g7c83nsbnrffkyrn1wmd7zzqdn";
      };
      reflex = pkgs.fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex";
        rev = "273bc3a019b479cc4c0ec522ca1d79207c339c8a";
        sha256 = "0j5251g0mqwqap3cls0pbnm1qk2nrsq5d4n59x550wihbiknpjsp";
      };
      reflex-dom = pkgs.fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex-dom";
        rev = "56aa06e94e5c9c6c2eb19cd988ed8de0d76cae38";
        sha256 = "0di64r4pl6kbxiayl1wp2vi9rfbzyj6mkyfs6qnm49kx6fm1zp4a";
      };
    };

    srcs = {
      constraints-extras = pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "constraints-extras";
        rev = "abd1bab0738463657fc6303e606015a97b01c8a0";
        sha256 = "0lpc3cy8a7h62zgqf214g5bf68dg8clwgh1fs8hada5af4ppxf0l";
      };

      gargoyle = libSelf.repos.gargoyle + /gargoyle;
      gargoyle-postgresql = libSelf.repos.gargoyle + /gargoyle-postgresql;

      groundhog = libSelf.repos.groundhog + /groundhog;
      groundhog-postgresql = libSelf.repos.groundhog + /groundhog-postgresql;
      groundhog-th = libSelf.repos.groundhog + /groundhog-th;

      rhyolite-aeson-orphans = ./aeson-orphans;
      rhyolite-backend = ./backend;
      rhyolite-backend-db = ./backend-db;
      rhyolite-backend-snap = ./backend-snap;
      rhyolite-common = ./common;
      rhyolite-datastructures = ./datastructures;
      rhyolite-frontend = ./frontend;

      monoidal-containers = pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "monoidal-containers";
        rev = "597c331238f9654541f567232d018c64d3ea147b";
        sha256 = "16qjpi9nr534fsxvnxzjv2sdg04874g16lf076m63jp92qx09yih";
      };

      reflex = libSelf.repos.reflex;

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
        inherit (import libSelf.repos.reflex-dom self pkgs) reflex-dom reflex-dom-core;
        gargoyle-postgresql-nix = pkgs.haskell.lib.addBuildTools
          (self.callCabal2nix "gargoyle-postgresql-nix" (libSelf.repos.gargoyle + /gargoyle-postgresql-nix) {})
          [ pkgs.postgresql ]; # TH use of `staticWhich` for `psql` requires this on the PATH during build time.
        heist = pkgs.haskell.lib.doJailbreak super.heist;
        pipes-binary = pkgs.haskell.lib.doJailbreak super.pipes-binary;
      });
  });

  proj = { pkgs ? import <nixpkgs> {} }:
    let
      obeliskImpl = pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "obelisk";
        rev = "ffce7c5f17e164f64cdae15895b30cefebbd7095";
        sha256 = "0hyhm9s52skmm7la06l1skg5casp3q0jympkg6k55qydh8ifshvx";
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

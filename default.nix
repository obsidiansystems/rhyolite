{ frontend ? false }:
let
  obelisk-src = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "obelisk";
    rev = "6c38599615eddba1b9f8dfb845f7404f53ed8053";
    sha256 = "0pqhppn2cb69v7r6wbg2zrx5ylxbgq7bl7qilarfbmyd9gcph9h4";
  };
  reflex-platform = (import obelisk-src {}).reflex-platform;

  gargoyle-src = reflex-platform.nixpkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "gargoyle";
    rev = "2c19c569325ad76694526e9b688ccdbf148df980";
    sha256 = "0257p0qd8xx900ngghkjbmjnvn7pjv05g0jm5kkrm4p6alrlhfyl";
  };
  groundhog-src = reflex-platform.nixpkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "groundhog";
    rev = "c2f18be45e3233f6268c8468eb0732dd6b2e8009";
    sha256 = "1r9i78bsnm6idbvp87gjklnr10g7c83nsbnrffkyrn1wmd7zzqdn";
  };
in reflex-platform.project ({ pkgs, ... }: {
  packages = {
    rhyolite-backend = ./backend;
    rhyolite-backend-snap = ./backend-snap;
    rhyolite-common = ./common;
    rhyolite-frontend = ./frontend;
    rhyolite-datastructures = ./datastructures;
    rhyolite-aeson-orphans  = ./aeson-orphans;

    groundhog = groundhog-src + /groundhog;
    groundhog-postgresql = groundhog-src + /groundhog-postgresql;
    groundhog-th = groundhog-src + /groundhog-th;

    obelisk-asset-serve-snap = obelisk-src + /lib/asset/serve-snap;
    obelisk-snap-extras = obelisk-src + /lib/snap-extras;
  };
  overrides = self: super: {
    gargoyle = self.callCabal2nix "gargoyle" (gargoyle-src + /gargoyle) {};
    gargoyle-postgresql-nix = pkgs.haskell.lib.addBuildTools
      (self.callCabal2nix "gargoyle-postgresql-nix" (gargoyle-src + /gargoyle-postgresql-nix) {})
      [ pkgs.postgresql ]; # TH use of `staticWhich` for `psql` requires this on the PATH during build time.
    gargoyle-postgresql = self.callCabal2nix "gargoyle-postgresql" (gargoyle-src + /gargoyle-postgresql) {};

    websockets = self.callCabal2nix "websockets" (pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "websockets";
      rev = "1493961d12c30c786b568df09d285582bc649fbc";
      sha256 = "17gf1xpj57gskigczxl7pk6n5iz6lbq3p8395755v1kfl37cdb5a";
    }) {};
    # Needed?
    heist = pkgs.haskell.lib.doJailbreak super.heist; # allow heist to use newer version of aeson
  };
  shells = rec {
    ghc = (if frontend then [] else [
      "rhyolite-backend"
      "rhyolite-backend-snap"
    ]) ++ ghcjs;
    ghcjs = [
      "rhyolite-common"
      "rhyolite-frontend"
    ];
  };
  tools = ghc: [ pkgs.postgresql ];
})

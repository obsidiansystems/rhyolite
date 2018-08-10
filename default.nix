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
    rev = "72355581ef1f8663e2772b31b90dc074d32a93a1";
    sha256 = "08w7aa5mb49ypqi8nhlrpd8jcm10h4ja9xsmb8b8mvcch6sk1ykf";
  };
  groundhog-src = reflex-platform.nixpkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "groundhog";
    rev = "c2f18be45e3233f6268c8468eb0732dd6b2e8009";
    sha256 = "1r9i78bsnm6idbvp87gjklnr10g7c83nsbnrffkyrn1wmd7zzqdn";
  };
in reflex-platform.project ({ pkgs, ... }: {
  packages = {
    # rhyolite-backend needs custom dependency configuration; see below.
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

    # NB: On the backend we depend on a fork of `websockets` which is used for `websockets-snap` as well.
    rhyolite-backend = self.callCabal2nix "rhyolite-backend" ./backend { websockets = self.websockets-obsidian; };

    websockets-obsidian = self.callCabal2nix "websockets-obsidian" (pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "websockets";
      rev = "62954d82401a9a2304a14a49973bd8c33db6a8f2";
      sha256 = "1cglrx6pbl5mdgfcsds5w2y1s4i9j375b3xim33jydr5g6c9ss4z";
    }) {};
    websockets-snap = self.callCabal2nix "websockets-snap" (pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "websockets-snap";
      rev = "0587aaeab9f9005d45b221c8ccc08b42dde9f900";
      sha256 = "0s07f9sdn98h88kxkv8jr455a559c43c8ybdyvbv5c94ipbz7pjj";
    }) { websockets = self.websockets-obsidian; };

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

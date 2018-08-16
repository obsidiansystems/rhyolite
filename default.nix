{ frontend ? false }:
let
  obelisk-src = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "obelisk";
    rev = "ffce7c5f17e164f64cdae15895b30cefebbd7095";
    sha256 = "0hyhm9s52skmm7la06l1skg5casp3q0jympkg6k55qydh8ifshvx";
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
    # rhyolite-backend needs custom dependency configuration; see below.
    rhyolite-backend-db = ./backend-db;
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

    monoidal-containers = pkgs.fetchFromGitHub {
      owner = "danbornside";
      repo = "monoidal-containers";
      rev = "f9bbf89b0f59ebcccbf116beefb26ce6d416cc69";
      sha256 = "1pbhprjsg5yh9483hgspy32smv9mbrkh16zdgmpw0qm9vmlw95cx";
    };

    constraints-extras = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "constraints-extras";
      rev =  "abd1bab0738463657fc6303e606015a97b01c8a0";
      sha256 = "0lpc3cy8a7h62zgqf214g5bf68dg8clwgh1fs8hada5af4ppxf0l";
    };
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
      "rhyolite-aeson-orphans"
      "rhyolite-backend"
      "rhyolite-backend-db"
      "rhyolite-backend-snap"
      "rhyolite-datastructures"
    ]) ++ ghcjs;
    ghcjs = [
      "rhyolite-common"
      "rhyolite-frontend"
    ];
  };
  tools = ghc: [ pkgs.postgresql ];
})

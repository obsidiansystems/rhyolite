# Hacking


## On backend

```bash
nix-shell -A shells.ghc --run 'cabal new-repl lib:rhyolite-backend'
```


## On frontend

```bash
cd frontend
nix-shell .. -A shells.ghc --arg frontend true --run 'cabal new-repl'
```
# Using Rhyolite with Obelisk

Within an already initialized Obelisk project,add to the project's `default.nix`  :
 1. fetch rhyolite from GitHub
 1. override the `packages` set and include variables to individual packages of rhyolite
 1. In order to use rhyolite-backend you will need to cabal override the following packages:
    1. gargoyle
    1. websockets
    1. websockets-snap

Here is a working example:
```
{ system ? builtins.currentSystem
, iosSdkVersion ? "10.2"
}:
let
  obelisk = import .obelisk/impl { inherit system; };
in
obelisk.project ./. ({ pkgs, ... }:
  let
    rhyolite-src = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "rhyolite";
      rev = "c8456661a6d6fa30efbef7c50394353c13c65e53";
      sha256 = "0s8wmkq2ivr2wj6srp4qx4388ffrqfhhgxyfw42im8vfjyfp83ld";
    };
  in {

    packages = {
      rhyolite-backend-snap = rhyolite-src + /backend-snap;
      rhyolite-common = rhyolite-src + /common;
      rhyolite-frontend = rhyolite-src + /frontend;
    };

		#cabal overrides for use of rhyolite-backend
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
		};

    android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    android.displayName = "Obelisk Minimal Example";
    ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    ios.bundleName = "Obelisk Minimal Example";
  })
```

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
    android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    android.displayName = "Obelisk Minimal Example";
    ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    ios.bundleName = "Obelisk Minimal Example";
  })
```

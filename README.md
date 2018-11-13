# Hacking


## On backend

```bash
nix-shell -A proj.shells.ghc --run 'cabal new-repl lib:rhyolite-backend'
```


## On frontend

```bash
cd frontend
nix-shell .. -A proj.shells.ghc --run 'cabal new-repl'
```
# Using Rhyolite with Obelisk

Within an already initialized Obelisk project, add to the project's `default.nix`:
 1. Fetch rhyolite from GitHub
 1. Compose Rhyolite's overrides with yours.

Example:
```
{ system ? builtins.currentSystem
, iosSdkVersion ? "10.2"
}@args:
let
  obelisk = import .obelisk/impl args;
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
      # your packages...
    };
    overrides = pkgs.lib.composeExtensions ((import rhyolite-src).lib args).haskellOverrides (self: super: {
      # your overrides
      # Rhyolites overrides will appear in `super`.
    });
    android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    android.displayName = "Obelisk Minimal Example";
    ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    ios.bundleName = "Obelisk Minimal Example";
  }
```

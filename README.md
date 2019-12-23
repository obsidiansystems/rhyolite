# Using Rhyolite with an Obelisk project

1. Within an already initialized Obelisk project, clone Rhyolite into
   the dep directory, if you haven’t already:

```bash
git clone https://github.com/obsidiansystems/rhyolite dep/rhyolite
```

1. Add Rhyolite’s haskellOverrides to default.nix so that your package
   can access them. This involves adding overrides to the arguments
   passed to Obelisk’s project function so that it imports Rhyolite’s
   haskellOverrides. You can base it off of this example:

```
{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, hackGet, ... }@args: {

  overrides = pkgs.lib.composeExtensions (import (hackGet ./dep/rhyolite) args).haskellOverrides
    (self: super: with pkgs.haskell.lib; {
      # Your custom overrides go here.
    });

  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
})
```

1. You can now add any of Rhyolite’s packages as dependencies to your
   Obelisk project. Here is the full list of packages provided:

   - rhyolite-aeson-orphans
   - rhyolite-backend
   - rhyolite-backend-db
   - rhyolite-backend-snap
   - rhyolite-common
   - rhyolite-datastructures
   - rhyolite-frontend

## Alternative method

You can also let Rhyolite manage Obelisk directly. This is easier to
setup but also means that you are stuck with the Obelisk version used
by Rhyolite.

To do this, simply overwrite the ```.obelisk/impl/github.json``` file
with this Rhyolite thunk:

```
{
  "owner": "obsidiansystems",
  "repo": "rhyolite",
  "branch": "master",
  "rev": "06b9851a101408a86a4ec0b7df5b2f71bc532ab0",
  "sha256": "18adbc1nnj94qhggpcxmpd5i1rz0zx93cpphl09mw4c7s65rzah7"
}
```

# Hacking


## On backend

```bash
nix-shell -A proj.shells.ghc --run 'cabal new-repl lib:rhyolite-backend'
```


## On frontend

```bash
nix-shell -A proj.shells.ghc --run 'cabal new-repl lib:rhyolite-frontend'
```

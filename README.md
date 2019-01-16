# Using Rhyolite with Obelisk

Within an already initialized Obelisk project, add these lines to the
project's `default.nix`:

```
  overrides = (import (builtins.fetchTarball {
    url = "https://github.com/obsidiansystems/rhyolite/archive/78c4d06b3c2cdee3527e06d270e345f403bbe428.tar.gz";
    sha256 = "11vjgmn8x09cgj71ddwnmqz5yf3lh689n1bf88wydbl1286fx2y7";
  }) {}).haskellOverrides;
```

They should be placed between the ending braces so that an unmodified
Obelisk project would look like:


```
{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  overrides = (import (builtins.fetchTarball {
    url = "https://github.com/obsidiansystems/rhyolite/archive/78c4d06b3c2cdee3527e06d270e345f403bbe428.tar.gz";
    sha256 = "11vjgmn8x09cgj71ddwnmqz5yf3lh689n1bf88wydbl1286fx2y7";
  }) {}).haskellOverrides;
})
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

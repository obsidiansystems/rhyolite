# <img src="./rhyolite.svg" width="48">  Rhyolite
[![Obelisk](https://img.shields.io/badge/Powered%20By-Obelisk-black?style=flat&logo=data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI3NjgiIGhlaWdodD0iNzY4Ij48ZyBmaWxsLXJ1bGU9ImV2ZW5vZGQiPjxwYXRoIGQ9Ik0zMDUuODggNjIyLjY3M2MtMzcuOTI0LTEyLjM4Ni03MS44NzMtMzMuNTU2LTk5LjQzNS02MS4xMThDMTYxLjAyIDUxNi4xMjkgMTMyLjk1MiA0NTMuMzQ0IDEzMi45NTIgMzg0YzAtNjkuMjU3IDI4LjA2Ny0xMzIuMTMgNzMuNDkzLTE3Ny41NTVDMjUxLjg3MSAxNjEuMDIgMzE0LjY1NiAxMzIuOTUyIDM4NCAxMzIuOTUyYzY5LjM0NCAwIDEzMi4xMyAyOC4wNjcgMTc3LjU1NSA3My40OTNDNjA2Ljk4IDI1MS44NzEgNjM1LjA0OCAzMTQuNzQzIDYzNS4wNDggMzg0YzAgNjkuMzQ0LTI4LjA2NyAxMzIuMTMtNzMuNDkzIDE3Ny41NTVDNTE2LjEyOSA2MDYuOTggNDUzLjM0NCA2MzUuMDQ4IDM4NCA2MzUuMDQ4VjE2MS4zNWwtMzkuNjEgMzIuMDU2LTM4LjUxIDQyOS4yNjYiIGZpbGw9IiMyZDMyM2IiLz48cGF0aCBkPSJNMzg0IDYwNi42NDdjNjEuNDk5IDAgMTE3LjE3OS0yNC44OTUgMTU3LjQ2NS02NS4xODJDNTgxLjc1MiA1MDEuMTggNjA2LjY0NyA0NDUuNSA2MDYuNjQ3IDM4NGMwLTYxLjQyNS0yNC44OTUtMTE3LjE3OS02NS4xODItMTU3LjQ2NUM1MDEuMTggMTg2LjI0OCA0NDUuNSAxNjEuMzUzIDM4NCAxNjEuMzUzdjQ0NS4yOTQiIGZpbGw9IiM3MDllYjUiLz48cGF0aCBkPSJNMzg0IDYzNS4wNDhjMjYuOTkgMCA1My41NjQtNC4yMzYgNzkuMjI1LTEyLjc5TDQyMy42MTMgMTkzLjQxIDM4NCAxNjEuMzUzdjQ3My42OTUiIGZpbGw9IiMyZDMyM2IiLz48L2c+PC9zdmc+)](https://github.com/obsidiansystems/obelisk) [![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Built with Nix](https://img.shields.io/static/v1?logo=nixos&logoColor=white&label=&message=Built%20with%20Nix&color=41439a)](https://nixos.org) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/obsidiansystems/rhyolite/blob/master/LICENSE)

Rhyolite is a collection of packages used to jump-start web application
development using Obelisk and reflex.

Rhyolite provides:
* a client/server communication framework for both request/response APIs and
  live-updating queries; and
* a library of types and components commonly used when building a web
  application.

## Using Rhyolite with an Obelisk project

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
   { system ? builtins.currentSystem, obelisk ? import ./.obelisk/impl {
     inherit system;
     iosSdkVersion = "13.2";

     # You must accept the Android Software Development Kit License Agreement at
     # https://developer.android.com/studio/terms in order to build Android apps.
     # Uncomment and set this to `true` to indicate your acceptance:
     # config.android_sdk.accept_license = false;

     # In order to use Let's Encrypt for HTTPS deployments you must accept
     # their terms of service at https://letsencrypt.org/repository/.
     # Uncomment and set this to `true` to indicate your acceptance:
     # terms.security.acme.acceptTerms = false;
   } }:
   with obelisk;
   project ./. ({ pkgs, hackGet, ... }@args: {

     overrides = pkgs.lib.composeExtensions
       (pkgs.callPackage (hackGet ./dep/rhyolite) args).haskellOverrides
       (self: super:
         with pkgs.haskell.lib;
         {
           # Your custom overrides go here.
         });

     android.applicationId = "systems.obsidian.obelisk.examples.minimal";
     android.displayName = "Obelisk Minimal Example";
     ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
     ios.bundleName = "Obelisk Minimal Example";
   })
   ```

1. You can now add any of Rhyolite’s packages as dependencies to your
   Obelisk project. The packages Rhyolite provides are listed in [cabal.project](cabal.project).

### Alternative method

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
  "private": false,
  "rev": "9f13d8d8a2233aae54e15c39acf68181893b859a",
  "sha256": "1vhbw9bdqpfddavfjfdrq6kk5wwsd8hbgb8pnna9i2db3x3cmzvy"
}
```

## Hacking

### Entering a shell

You can use `nix-shell -A proj.shells.ghc` to enter a shell from which you can build any of the subprojects in this repository. From that shell you can:

* Enter a shell using `cabal repl /path/to/package`
* Use ghcid by invoking `ghcid -c "cabal repl /path/to/package`
* Build a package using `cabal build /path/to/package`
* Generate haddock documentation using `cabal haddock /path/to/package` (e.g., `cabal haddock notify-listen/notify-listen-beam`)
* Generate a standalone haddock page for all the packages in this repo by running `./gen-docs.sh`

### Testing that things build

Because of the inter-related nature of these packages, `rhyolite-test-suite` tests that all of them can be built against one another. To test, run:

```bash
nix-shell -A proj.shells.ghc --run "cabal build test"
```

{
  self-args ? {
    config.android_sdk.accept_license = true;
    iosSdkVersion = "10.2";
  }
, local-self ? import ./. self-args
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
}:
let
  inherit (local-self) reflex-platform;
  inherit (local-self.nixpkgs) lib;

  perPlatform = lib.genAttrs supportedSystems (system: let
    self = import ./. (self-args // { inherit system; });
    reflex-platform = self.reflex-platform;
    cachePackages =
      (builtins.attrValues (self.rhyolitePackages self.obelisk))
      ++ [
        self.proj.ghcjs.rhyolite-frontend
        (import ./. {}).proj.ghc.rhyolite-test-suite
      ];
  in {
    cache = reflex-platform.pinBuildInputs "rhyolite-${system}" cachePackages;
  });

  metaCache = reflex-platform.pinBuildInputs "rhyolite-everywhere"
    (map (a: a.cache) (builtins.attrValues perPlatform));

in perPlatform // {
  inherit metaCache;
}

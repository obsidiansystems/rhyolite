{
  self-args ? {
    config.android_sdk.accept_license = true;
  }
, local-self ? import ./. self-args
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
}:
let
  inherit (local-self.nixpkgs) lib;

  perPlatform = lib.genAttrs supportedSystems (system: let
    self = import ./. (self-args // { inherit system; });
    cachePackages =
      (builtins.attrValues self.rhyolitePackages)
      ++ [
        self.proj.ghcjs.rhyolite-frontend
        self.proj.ghc.rhyolite-test-suite
      ];
  in self.proj // {
    recurseForDerivations = true;
  });

in perPlatform // {
  recurseForDerivations = true;
}

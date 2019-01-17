let
  lib = (import ./. {}).nixpkgs.lib;

  systems = [ "x86_64-linux" "x86_64-darwin" ];

in

  lib.genAttrs systems (system: let
    rhyolite = import ./. { inherit system; };
    reflex-platform = rhyolite.reflex-platform;
  in {

    inherit (rhyolite) proj;

  })

let
  obelisk = import ./.obelisk/impl {};
  inherit (obelisk.nixpkgs) lib;

  systems = [ "x86_64-linux" "x86_64-darwin" ];

in

  lib.genAttrs systems (system: let
    rhyolite = import ./. { inherit system; };
  in {
    inherit (rhyolite) proj;
  })

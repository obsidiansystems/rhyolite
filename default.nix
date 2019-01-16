{ ... } @ args:

let

  obelisk = import ./.obelisk/impl args;
  reflex-platform = obelisk.reflex-platform;
  pkgs = obelisk.nixpkgs;
  inherit (pkgs) lib;

  # You can use these manually if you don’t want to use rhyolite.project.
  # It will be needed if you need to combine with multiple overrides.
  haskellOverrides = import ./overlay.nix { inherit pkgs; };

in obelisk // {

  inherit haskellOverrides;

  # Function similar to obelisk.project that handles overrides for you.
  project = base: projectDefinition:
    obelisk.project base ({...}@args:
      let def = projectDefinition args;
      in def // {
        overrides = lib.composeExtensions haskellOverrides (def.overrides or (_: _: {}));
      });

}

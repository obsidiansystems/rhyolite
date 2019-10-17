# Applies a given function to the subdirectories of this one.
f: let inherit (builtins) readDir filter map listToAttrs getAttr attrNames;
       fs = readDir ./.;
       dirs = filter (x: getAttr x fs == "directory") (attrNames fs);
   in listToAttrs (map (d: { name = d; value = f (./. + ("/" + d)); }) dirs)

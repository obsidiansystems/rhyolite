{ mkDerivation, aeson, async, attoparsec, base, bytestring
, constraints, constraints-extras, containers, data-default
, dependent-map, dependent-sum, file-embed, filepath, http-types
, mime-mail, monad-control, monad-logger, monoidal-containers, mtl
, network-uri, reflex, resource-pool, stdenv, template-haskell
, text, these, time, transformers, transformers-base, vector
, aeson-gadt-th, database-id-class, constraints-extras
, hostPlatform
}:
mkDerivation {
  pname = "rhyolite-common";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson async attoparsec base bytestring constraints
    constraints-extras containers data-default dependent-map
    dependent-sum file-embed filepath http-types mime-mail
    monad-control monoidal-containers mtl network-uri
    reflex resource-pool template-haskell text these time transformers
    transformers-base vector aeson-gadt-th constraints-extras
  ] ++ (if hostPlatform.libc == "bionic" || hostPlatform.isAarch64 then [] else [
    monad-logger
  ]);
  license = stdenv.lib.licenses.bsd3;
}

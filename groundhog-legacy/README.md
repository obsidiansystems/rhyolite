# rhyolite-groundhog-legacy

Legacy adapters for groundhog-based rhyolite projects.

## Purpose

This package provides legacy support for groundhog as used in rhyolite as of the [2021-11-18 release](https://github.com/obsidiansystems/rhyolite/tree/release/2021-11-18).

For other changes, unrelated to groundhog, please consult the rhyolite changelog.

## Migration

### Rhyolite.Map.Monoidal

Instead of importing `Rhyolite.Map.Monoidal`, you should just use `Data.Map.Monoidal` and use monoidal-containers version 0.6.1.0 or newer, which provides the `restrictKeys` function that this module used to provide. The convenience constructor `=:` has been removed: either define it locally or replace it with `Data.Map.Monoidal.singleton`.

### Rhyolite.Backend.DB.PsqlSimple

### PostgresRaw

`PostgresRaw` has been renamed to `Psql` and can now be found in `psql-simple-class`.

Groundhog instances for `Psql` are now in `psql-simple-groundhog`.

### FromField and ToField for `Id`

These instances have been moved to `Rhyolite.DB.Groundhog.Orphans` in this package.

### fromIdRow

This function has been removed.

### Rhyolite.Backend.Listen

This module has been moved to `Rhyolite.DB.NotifyListen.Groundhog` in this package.

### Database.Groundhog.Postgresql.Orphans

These orphans have been moved to `Rhyolite.DB.Groundhog.Orphans` in this package.

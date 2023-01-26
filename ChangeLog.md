# Revision history for Rhyolite

This project's release branch is `master`. This log is written from the perspective of the release branch: when changes hit `master`, they are considered released, and the date should reflect that release.

## 2023-01-26
* Breaking: Rhyolite.Frontend.Cookie now always Base64 encodes cookies
* change taskWorker to not manage the hasRun flag.  For the old behavior, use `taskWorker1` which adds back the at-most-once execution behavior.  The old `_task_hasRun` field of Task is a separate argument.
* Task type no longer uses lenses for field accessors.  use field labels or regular functions instead.
* Fix `Rhyolite.DB.Beam.current_timestamp_` for PostgreSQL server instances whose time zone is *not* set to UTC.
* Add helper types for beam code, namely `WrapColumnar` and `WrapNullable`.
* Add beam orphan instances for some functor types, such as `Product` and `Proxy`.
* Bump obelisk 1.0.0.0 and reflex-platform (0.9.2.0)
* Add rhyolite-account-types and rhyolite-account-backend for use with beam-based rhyolite projects
  * Moved groundhog-legacy's `Rhyolite.Account` module to `Rhyolite.Account.Groundhog.Types`
* Added Beam versions of `Rhyolite.Task.Groundhog.Worker.taskWorker` and `Rhyolite.Task.Groundhog.Task`, in `Rhyolite.Task.Groundhog.Worker` and `Rhyolite.Task.Beam` respectively.
* Removed `Rhyolite.Request.Common`, now using functions from Aeson directly. Use `decode'` instead of `decodeValue'`.
* Bump vessel to 0.2.1.0 (pre-release)
* Add mapAuthenticatedV
* Extend AuthenticatedV to have a personal view component in addition to private and public ones. Personal views are similar to private ones
  in that they require a token to access, but are computed separately per-user.
  They're specifically for the case that we expect different users' views to have nothing in common with one another.

* Breaking changes:
  * Rename `Rhyolite.Frontend.App.openWebSocket'` to `openWebSocket` and remove the previous definition of `openWebSocket`. To get the same output as before: `(_appWebSocket_notification x, _appWebSocket_response x)`.
  * Remove the `rhyolite-backend-snap` package. It has been released as [snap-stream](https://hackage.haskell.org/package/snap-stream). Use that package instead.
  * Remove the `backend-db` package. See the [groundhog-legacy migration guide](groundhog-legacy/README.md) and the notes below:
    * Remove `Rhyolite.Backend.DB.PsqlSimple.Orphans`. Changes have been moved our fork of [postgresql-simple](https://github.com/obsidiansystems/postgresql-simple).
    * Rename `PostgresRaw` to `Psql` and move it to `psql-simple-class`.
    * Move Psql (formerly PostgresRaw) instance for groundhog's `DbPersist` to psql-simple-groundhog.
    * Remove `fromIdRow` from `Rhyolite.Backend.DB.PsqlSimple`
    * Move `traceQuery` and `traceExecute` to psql-simple-class
    * Use `postgresql-simple-interpolate` for quasiquotated postgresql-simple queries. The following function names have changed:
      * queryQ -> iquery
      * executeQ -> iexecute
      * executeQ_ -> iexecute_
      * traceQueryQ -> itraceQuery (defined in psql-simple-class)
      * traceExecuteQ -> itraceExecute (defined in psql-simple-class)
      * traceExecuteQ_ -> itraceExecute_ (defined in psql-simple-class)
    * Remove `Rhyolite.Backend.DB.PsqlSimple` as all of its functionality has been moved elsewhere.
    * Move instances from `Database.Groundhog.Postgresql.Orphans` to `Rhyolite.DB.Groundhog.Orphans` in groundhog-legacy.
    * Move `Network.PushNotification.Worker` to `groundhog-legacy` and rename it to `Rhyolite.Network.PushNotification.Worker`.
    * Move `Rhyolite.Backend.DB` to `groundhog-legacy` and rename it to `Rhyolite.DB.Groundhog`.
    * Move the `PostgresLargeObject` class to `psql-simple-class` and move its groundhog-based instances to `groundhog-legacy`.
      * `withStreamedLargeObject` no longer requires groundhog, but does require a connection. Use `liftWithConn` to use it with groundhog.
    * Move `Rhyolite.Backend.DB.Serializable` to `Database.PostgreSQL.Serializable` in `psql-serializable` and move its groundhog-based instances to `groundhog-legacy`'s `Rhyolite.DB.Groundhog.Serializable` module.
    * Move `Rhyolite.Backend.Schema` and `Rhyolite.Backend.Schema.Class` to `Rhyolite.DB.Groundhog.Schema` and `Rhyolite.DB.Groundhog.Schema.Class` in `groundhog-legacy`.
    * Move `Rhyolite.Schema.Task` and `Rhyolite.Backend.Schema.Task` to `Rhyolite.Task.Groundhog`, and move `Rhyolite.Backend.TaskWorker` to `Rhyolite.Task.Groundhog.Worker`, both in `groundhog-legacy`.
    * Move `Rhyolite.Backend.Schema.TH` to `Rhyolite.DB.Groundhog.TH` in `groundhog-legacy`.
  * Remove `Rhyolite.Map.Monoidal`. For `=:` use `Data.Map.Monoidal.singleton` instead, and for `restrictKeys` use monoidal-containers >= 0.6.1.0.
  * Remove rhyolite-aeson-orphans. It has been renamed and moved to [bytestring-aeson-orphans](https://github.com/obsidiansystems/bytestring-aeson-orphans) and is now used as a dependency.
    * Remove the aeson orphan instances for Alt, Any and Down.
  * Move `Rhyolite.Backend.Listen` to its own project `rhyolite-notify-listen`. The module is now called `Rhyolite.DB.NotifyListen`. `insertAndNotify` and related classes and functions can now be found in the groundhog-legacy package in the `Rhyolite.DB.NotifyListen.Groundhog` module, and in `notify-listen-beam` for beam versions. The various `notify` functions now require `Psql m`.
  * Remove Rhyolite.HList.
  * Remove Data.MonoidMap. It has been moved to [monoid-map](https://github.com/obsidiansystems/monoid-map) and is now used as a dependency.
  * Narrow the type of `signWithKey` so that the input type matches the output's phantom type parameter.
  * Move `Rhyolite.Backend.Email` and `Rhyolite.Email` to `Rhyolite.Email` in the new `rhyolite-email` package.
    * Provide more error information in the interface for sending an email in `rhyolite-email`.
    * The `EmailEnv` type has been changed to `EmailConfig`, which is a structured record type instead of a flat tuple.
  * Move `LargeObjectId` to `psql-simple-class`.
  * Remove the `Rhyolite.TH` module. Use `file-embed` instead.
  * Move `Rhyolite.Backend.EmailWorker` to `Rhyolite.DB.Groundhog.EmailWorker` in `groundhog-legacy`. Change the schema for `QueuedEmail` to contain a JSON blob of the `Mail` object instead of the rendered email bytestring. See the groundhog-legacy migration guide for more information.
  * Move the `Account` type to `groundhog-legacy-types` and move associated backend functionality to `Rhyolite.DB.Groundhog.Account` in `groundhog-legacy`.
  * Move `Signed` and `MonadSign` to `signed-data`. Move `signWithKey`, `readSignedWithKey`, `SignT`, etc to `signed-data-clientsession`. Move groundhog instances for `MonadSign` and `SignT` to `Rhyolite.DB.Groundhog.Orphans`.
  * Move `Account` to `groundhog-legacy-types`. Move backend code for accounts, e.g., `ensureAccountExists` to `Rhyolite.Account.Groundhog` in `groundhog-legacy`.
  * Move `Rhyolite.Schema` to `groundhog-legacy-types`.
  * Rename `datastructures` to `semimap` since it only contains SemiMap.
  * Move `Rhyolite.Frontend.Widget`, `Rhyolite.Frontend.Form`, and `Rhyolite.Frontend.Modal.*` to `rhyolite-widgets`.
* New:
  * Add a `Psql` instance for beam's `Pg`
* Version bumps:
  * vessel 0.2.0.0

## 2021-11-18

* Fixed a bug in `handleAuthMapQuery` where tokens were always validated

## 2021-11-16
* Rhyolite.Concurrent: add taggedWorker to make it easier to determine the source of error messages
* Bump groundhog
* Bump obelisk (develop as of 2021-04-19) and reflex-platform (0.7.1.0)
* Remove rhyolite-logging and use monad-logger-extras in rhyolite-db instead. `LoggingEnv` is replaced by `Logger`.
* Remove `runPrerenderedRhyoliteWidget`. Use `runRhyoliteWidget` instead.
* `runObeliskRhyoliteWidget` now also returns a Dynamic `AppWebSocket`.
* Emails
  * separate build and send for widget email
  * Update widget emails for newer routes.
  * Use a record for email configuration.
  * Add a function for email with a StaticWidget
* Simple authenticated queries
  * `ErrorV` vessel captures logic for possibly failing queries
  * `AuthMapV` gathers queries associated to different identities so that they can be processed together.
  * `AuthenticatedV` distinguishes between public queries that need no authentication and private queries which do.

## 2020-04-28

* Bump Obelisk to version 0.8 which includes a bump to reflex-platform and nixpkgs 19.09

## 2020-01-10

* Add `mapModalTM`, `mapModalT` and `mapModalTM` functions for changing the underlying monads of `ModalT`.
* Fix bug in `functorToWire` causing `SelectedCount` of `0` to be considered non-mempty.
* Fix regression in rhyolite-frontend so that it builds in GHCJS again

## 2019-12-20

* Move `Rhyolite.Backend.Logging` to its own project `rhyolite-logging` and re-export.
* Add `Rhyolite.Backend.DB.Serializable` for doing PostgreSQL transactions in `SERIALIZABLE` isolation with automatic retrying on serialization errors.
* *Breaking change*: Switch `runDb` to use the `Serializable` monad for all transactions. This monad is not a transformer and has very few instances. Notably, lifting `IO` into it must be done *manually*, and only with great care.
* Add `runDbReadOnlyRepeatableRead` to `RunDb` to make streaming reads easier. Notably, this transaction mode allows `MonadIO` because it will never retry.
* Fix thread-safety bug in `firebaseWorker`.
* *Breaking change*: Rework `MonadSign` to look a lot more like `MonadReader` so that its instances can more easily avoid `MonadIO`.
* *Breaking change*: Functions in `Rhyolite.Backend.EmailWorker` now have a constraint requiring that `Serializable` be the base monad.
* Add `PostgresRaw` and `PostgresLargeObject` instances to `SignT`.
* Bump `postgresql-simple` to avoid `WARNING: There is no transaction in progress` when there is a serialization error.

## 2019-10-20

* Deprecated runPrerenderedRhyoliteWidget, use runRhyoliteWidget instead.
* Use GHC 8.6. This necessitated bumps to
    * dependent-sum-aeson-orphans
    * gargoyle
    * groundhog
    * obelisk
    * postgresql-lo-stream
    * reflex-platform
    * vessel
* Convert many places using `fail` to use `error` directly to avoid `MonadFail` constraint
* Remove many instances from `Backend.DB.PsqlSimple.Orphans` that were upstreamed to `postgresql-simple`

## 2019-08-27

* Improve error message on parse failure in certain database instances

## 2019-08-19

* Added `runObeliskRhyoliteWidget` for integration with obelisk applications.

## 2019-08-02

* Remove `Rhyolite.Backend.Snap`. That module has been made obsolete by Obelisk.
* Frontend.App: Use reflex's `matchResponsesWithRequests` instead of `identifyTags` (now deleted). Change the identifier used by `TaggedRequest` and `TaggedResponse` to an `Int`
* Remove `mapRequesterT`. This has been upstreamed to reflex as `withRequesterT`
* Remove Frontend.Request and Frontend.WebSocket

## 2019-08-01

* Use `HasId`, `Id`, and related types/functions from database-id-class and database-id-groundhog. Remove the implementations in Rhyolite.Schema and Rhyolite.Backend.Schema
* Remove Rhyolite.Backend.DB.TH since the code there was redundant with Rhyolite.Backend.Schema.TH
* Convert all dependencies in default.nix to thunks in `dep/`
* Update to use monoidal-containers 0.5
* Add a test that builds all the rhyolite submodules to `test/`
* Remove `withDb`, which can now be found in [gargoyle-postgresql-connect](https://github.com/obsidiansystems/gargoyle/tree/develop/gargoyle-postgresql-connect)

## 2019-06-14

* Add `Rhyolite.Frontend.Modal.*` modules for conveniently creating modals that do not require the use of `z-index` to position correctly.
* Add `withLoggingMinLevel` function in `Rhyolite.Backend.Logging` which allows you to pick the fallback filter when no other filters match.
* Bump obelisk to a version that no longer uses `*Tag` classes.
* Remove the "HasView" and "HasRequest" classes, and the general concept of having a type level "app" identifier. Instead, everything is explicitly parametrised on query and request types directly, and the query type is no longer *required* to be a Functor/Align/etc. so that Vessel becomes an option for defining queries and views.
* Remove the "Request" class, as it has been subsumed by more general machinery. You can use deriveArgDict from constraints-extras and deriveJSONGADT from aeson-gadt-th on your request datatypes to obtain the same powers (and more).
* In its place, there is a Request type synonym which stands for (ForallF ToJSON r, Has ToJSON r, FromJSON (Some r), Has FromJSON r).
* Added standardPipeline as a good example of a last argument you can use for serveDbOverWebsockets, in the case that you have a Functor-style query/view type. It now uses condense/disperse from the Vessel library.
* Added vesselPipeline similarly for the case where you're using a functor-parametric container type (such as Vessel) for your queries and views.
* Added a DiffQuery type class which allows us to specify how queries are subtracted. We were doing this in an ad-hoc fashion based on Align instances before, but the generalisation of query types meant that we could no longer assume this was an option.
* If you have a Functor-style query/view, the 'standardDiffQuery' function can be used to implement the 'DiffQuery' instance for it.
* If you're using Vessel, to implement DiffQuery you can use subtractV which is a consequence of the View typeclass.
* Add alternative to groundhog's `==.`, which has severe performance issues before version 0.10 (to which we can't yet upgrade). See `Rhyolite.Backend.DB.===.`.
* Add `validationDropdown` and `validationTextArea`
  * Add an extra type parameter `v` specifying the widget value type (typically `Text`) to `ValidationConfig`
  * Add `mkValidationConfig` which is like `defValidationConfig` but takes the initial value
  * Add `validationDropdownChangeEvent` that returns the change event of the ValidationDropdown
* Remove the "HasView" and "HasRequest" classes, and the general concept of having a type level "app" identifier. Instead, everything is explicitly parametrised on query and request types directly, and the query type is no longer *required* to be a Functor/Align/etc. so that Vessel becomes an option for defining queries and views.
* Remove the "Request" class, as it has been subsumed by more general machinery. You can use deriveArgDict from constraints-extras and deriveJSONGADT from aeson-gadt-th on your request datatypes to obtain the same powers (and more).
* In its place, there is a Request type synonym which stands for (ForallF ToJSON r, Has ToJSON r, FromJSON (Some r), Has FromJSON r). 
* Added standardPipeline as a good example of a last argument you can use for serveDbOverWebsockets, in the case that you have a Functor-style query/view type. It now uses condense/disperse from the Vessel library.
* Added vesselPipeline similarly for the case where you're using a functor-parametric container type (such as Vessel) for your queries and views.
* Added a DiffQuery type class which allows us to specify how queries are subtracted. We were doing this in an ad-hoc fashion based on Align instances before, but the generalisation of query types meant that we could no longer assume this was an option.
* If you have a Functor-style query/view, the 'standardDiffQuery' function can be used to implement the 'DiffQuery' instance for it.
* If you're using Vessel, to implement DiffQuery you can use subtractV which is a consequence of the View typeclass.
* Bump obelisk to a version that no longer uses `*Tag` classes

## 2019-05-08

* Add NeverNull instance for our Json newtype that encodes things as JSON in the DB.
* Add change-tracking DB notifications. See `HasChangeNotification` in Rhyolite.Backend.Listen.
* Fix bug in `runRhyoliteWidget` and `runPrerenderedRhyoliteWidget` when things were explicitly requested 0 times.
* Fix `mapAuth` to ensure that queries are correctly subtracted from the view selector when switching accounts.
* Add `validationInputWithFeedback` which separates the widget providing feedback on why their input wasn't valid, allowing it to be placed separately in the DOM.
* Create a `RawTask`, useful for jobs where it's inconvenient to track "job complete" separate from the other job ready conditions.

## v0.1.0.0

* Add a server-side worker to handle sending of push notifications
* Disallow using ExceptT and similar monads to host runDb calls
* Add `mapAuth` to rhyolite frontend to allow mapping of application credentials into an "authenticated" subwidget
* Update frontend for obelisk's new hydation feature
* Add Rhyolite.Frontend.Widget to collect useful widgets like `extensibleListWidget` - a widget for editing a list of values
* Add Rhyolite.Frontend.Form, a collection of widgets to help with form validation
* Backend notifications are now typed. This is a breaking change: you'll need to specify your expected notification as a GADT and supply the appropriate GADT constructor when constructing notifications
* Add a worker for handling unqueued tasks that the backend must run. See `Rhyolite.Backend.TaskWorker`

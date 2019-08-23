# Revision history for rhyolite

This project's release branch is `master`. This log is written from the perspective of the release branch: when changes hit `master`, they are considered released, and the date should reflect that release.

## 2019-08-19 - Unreleased

* Added `standardPipeline` as a good example of a last argument you can use for serveDbOverWebsockets, in the case that you have a Functor-style query/view type. It now uses condense/disperse from the Vessel library.
* Added `runObeliskRhyoliteWidget` for integration with obelisk applications.

## 2019-08-02 - Unreleased

* Remove `Rhyolite.Backend.Snap`. That module has been made obsolete by Obelisk.
* Frontend.App: Use reflex's `matchResponsesWithRequests` instead of `identifyTags` (now deleted). Change the identifier used by `TaggedRequest` and `TaggedResponse` to an `Int`
* Remove `mapRequesterT`. This has been upstreamed to reflex as `withRequesterT`
* Remove Frontend.Request and Frontend.WebSocket
* Add SQL QuasiQuote variants supporting anti-quotation of arbitrary Haskell expressions (`Rhyolite.Backend.DB.PsqlSimple.queryQi` et al.)

## 2019-08-01 - Unreleased

* Use `HasId`, `Id`, and related types/functions from database-id-class and database-id-groundhog. Remove the implementations in Rhyolite.Schema and Rhyolite.Backend.Schema
* Remove Rhyolite.Backend.DB.TH since the code there was redundant with Rhyolite.Backend.Schema.TH
* Convert all dependencies in default.nix to thunks in `dep/`
* Update to use monoidal-containers 0.5
* Add a test that builds all the rhyolite submodules to `test/`
* Remove `withDb`, which can now be found in [gargoyle-postgresql-connect](https://github.com/obsidiansystems/gargoyle/tree/develop/gargoyle-postgresql-connect)

## 2019-06-14 - Unreleased

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
* Bump obelisk to a version that no longer uses `*Tag` classes
* Add alternative to groundhog's `==.`, which has severe performance issues before version 0.10 (to which we can't yet upgrade). See `Rhyolite.Backend.DB.===.`.

## 2019-05-08 - Unreleased

* Add NeverNull instance for our Json newtype that encodes things as JSON in the DB.
* Add change-tracking DB notifications. See `HasChangeNotification` in Rhyolite.Backend.Listen.
* Fix bug in `runRhyoliteWidget` and `runPrerenderedRhyoliteWidget` when things were explicitly requested 0 times.
* Fix `mapAuth` to ensure that queries are correctly subtracted from the view selector when switching accounts.
* Add `validationInputWithFeedback` which separates the widget providing feedback on why their input wasn't valid, allowing it to be placed separately in the DOM.

## v0.1.0.0

* Add a server-side worker to handle sending of push notifications
* Disallow using ExceptT and similar monads to host runDb calls
* Add `mapAuth` to rhyolite frontend to allow mapping of application credentials into an "authenticated" subwidget
* Update frontend for obelisk's new hydation feature
* Add Rhyolite.Frontend.Widget to collect useful widgets like `extensibleListWidget` - a widget for editing a list of values
* Add Rhyolite.Frontend.Form, a collection of widgets to help with form validation
* Backend notifications are now typed. This is a breaking change: you'll need to specify your expected notification as a GADT and supply the appropriate GADT constructor when constructing notifications
* Add a worker for handling unqueued tasks that the backend must run. See `Rhyolite.Backend.TaskWorker`

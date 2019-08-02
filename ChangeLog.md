# Revision history for rhyolite

This project's release branch is `master`. This log is written from the perspective of the release branch: when changes hit `master`, they are considered released, and the date should reflect that release.

## 2019-08-02 - Unreleased

* Remove `Rhyolite.Backend.Snap`. That module has been made obsolete by Obelisk. 
* Frontend.App: Use reflex's `matchResponsesWithRequests` instead of `identifyTags` (now deleted). Change the identifier used by `TaggedRequest` and `TaggedResponse` to an `Int`
* Remove `mapRequesterT`. This has been upstreamed to reflex as `withRequesterT`
* Remove Frontend.Request and Frontend.WebSocket

## 2019-08-01 - Unreleased

* Use `HasId`, `Id`, and related types/functions from database-id-class and database-id-groundhog. Remove the implementations in Rhyolite.Schema and Rhyolite.Backend.Schema
* Remove Rhyolite.Backend.DB.TH since the code there was redundant with Rhyolite.Backend.Schema.TH
* Convert all dependencies in default.nix to thunks in `dep/`
* Update to use monoidal-containers 0.5
* Add a test that builds all the rhyolite submodules to `test/`
* Remove `withDb`, which can now be found in [gargoyle-postgresql-connect](https://github.com/obsidiansystems/gargoyle/tree/develop/gargoyle-postgresql-connect)

## 2019-06-14 - Unreleased

* Add `validationDropdown` and `validationTextArea`
  * Add an extra type parameter `v` specifying the widget value type (typically `Text`) to `ValidationConfig`
  * Add `mkValidationConfig` which is like `defValidationConfig` but takes the initial value
  * Add `validationDropdownChangeEvent` that returns the change event of the ValidationDropdown
* Add `withLoggingMinLevel` function in `Rhyolite.Backend.Logging` which allows you to pick the fallback filter when no other filters match.
* Bump obelisk to a version that no longer uses `*Tag` classes.
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

# Revision history for rhyolite

This project's release branch is `master`. This log is written from the perspective of the release branch: when changes hit `master`, they are considered released, and the date should reflect that release.

## 2019-08-27 - Unreleased

* Improve error message on parse failure in certain database instances

## 2019-06-14 - Unreleased

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

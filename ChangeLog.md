# Change log for Rhyolite

## 2019-05-08 - Unreleased

* NeverNull for our Json newtype that encodes things as JSON in the DB.
* Change-tracking DB notifications.
* Some bugfixes to run{Prerendered,}RhyoliteWidget for when things were explicitly requested 0 times.
* A fix to mapAuth to ensure that queries are correctly subtracted from the view selector when switching accounts.
* Add a validationInput which separates the widget providing feedback on why their input wasn't valid, allowing it to be
  placed separately in the DOM.

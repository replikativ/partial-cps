(ns is.simm.partial-cps.binding-capture-test-macros
  (:require [is.simm.partial-cps.ioc :as ioc]))

(ioc/register-binding-restorer!
 'is.simm.partial-cps.binding-capture-test/*context*
 'is.simm.partial-cps.binding-capture-test/restore-entry
 'is.simm.partial-cps.binding-capture-test/capture-entry)

(ioc/register-binding-restorer!
 'is.simm.partial-cps.binding-capture-test/*raw*
 'is.simm.partial-cps.binding-capture-test/restore-raw)

(ioc/register-binding-restorer!
 'is.simm.partial-cps.binding-capture-test/*captured-only*
 'is.simm.partial-cps.binding-capture-test/restore-entry
 'is.simm.partial-cps.binding-capture-test/capture-entry)

(ioc/register-binding-scope!
 'is.simm.partial-cps.binding-capture-test/*context*
 'is.simm.partial-cps.binding-capture-test/capture-scope
 'is.simm.partial-cps.binding-capture-test/restore-scope)

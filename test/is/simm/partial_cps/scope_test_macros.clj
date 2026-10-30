(ns is.simm.partial-cps.scope-test-macros
  (:require [is.simm.partial-cps.ioc :as ioc]))

(ioc/register-binding-restorer!
 'is.simm.partial-cps.scope-test/*context*
 'is.simm.partial-cps.scope-test/restore-exit)
(ioc/register-binding-scope!
 'is.simm.partial-cps.scope-test/*context*
 'is.simm.partial-cps.scope-test/capture-scope
 'is.simm.partial-cps.scope-test/restore-scope)

(defn syntax-handler [_ r _e]
  (fn [args] `(~r '~(first args))))
(defn ignore-handler [_ r _e]
  (fn [_] `(~r :ignored)))
(defmacro custom-effects [& body]
  (let [r (gensym) e (gensym)]
    `(fn [~r ~e]
       ~(ioc/invert {:env &env :r r :e e
                     :breakpoints {'is.simm.partial-cps.scope-test/syntax-effect
                                   'is.simm.partial-cps.scope-test-macros/syntax-handler
                                   'is.simm.partial-cps.scope-test/ignore-effect
                                   'is.simm.partial-cps.scope-test-macros/ignore-handler}}
                    (cons 'do body)))))

(ns is.simm.partial-cps.collection-macros
  (:require [is.simm.partial-cps.async :as a]
            [is.simm.partial-cps.ioc :as ioc]))

(defmacro hidden-await [task]
  (list 'is.simm.partial-cps.async/await task))

(defmacro wrapped-mapv [f xs]
  (list 'clojure.core/mapv f xs))

(defmacro rejection [form]
  (try
    (ioc/invert {:env &env :breakpoints a/breakpoints :r (gensym) :e (gensym)} form)
    nil
    (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(ns is.simm.partial-cps.closure-diagnostics-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.partial-cps.async :as a]
            [is.simm.partial-cps.ioc :as ioc]
            [is.simm.partial-cps.boundary-macros]))

(defn error-data [form]
  (binding [*ns* (the-ns 'is.simm.partial-cps.closure-diagnostics-test)]
    (try (eval form) nil
         (catch Throwable e
           (ex-data (last (take-while some? (iterate ex-cause e))))))))

(deftest opaque-effects-fail-at-compilation
  (doseq [form ['(a/async (mapv (fn [x] (a/await x)) []))
                '(a/async (mapv (fn [x] (is.simm.partial-cps.boundary-macros/hidden-await x)) []))
                '(a/async (vec (for [x [1 2]] (a/await x))))
                '(a/async (reify Runnable (run [_] (a/await :task))))]]
    (let [error (error-data form)]
      (is (= ::ioc/unsupported-suspension (:type error)))
      (is (some? (:site error)))))
  (is (nil? (error-data '(a/async (fn [await] (await :native-local))))))
  (is (nil? (error-data '(a/async (fn [] '(a/await :quoted))))))
  (is (nil? (error-data '(a/async (fn [] (a/async (a/await :nested-task))))))))

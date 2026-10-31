(ns is.simm.partial-cps.collection-diagnostics-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.partial-cps.async :as a]
            [is.simm.partial-cps.collections :as c]
            [is.simm.partial-cps.ioc :as ioc]
            [is.simm.partial-cps.collection-macros]))

(defn error-data [form]
  (binding [*ns* (the-ns 'is.simm.partial-cps.collection-diagnostics-test)]
    (try (eval form) nil
         (catch Throwable e
           (ex-data (last (take-while some? (iterate ex-cause e))))))))

(deftest opaque-effects-fail-at-compilation
  (doseq [form ['(a/async (mapv (fn [x] (a/await x)) []))
                '(a/async (mapv (fn [x] (is.simm.partial-cps.collection-macros/hidden-await x)) []))
                '(a/async (vec (for [x [1 2]] (a/await x))))
                '(a/async (reify Runnable (run [_] (a/await :task))))]]
    (let [error (error-data form)]
      (is (= ::ioc/unsupported-suspension (:type error)))
      (is (some? (:site error)))))
  (is (nil? (error-data '(a/async (fn [await] (await :native-local))))))
  (is (nil? (error-data '(a/async (fn [] '(a/await :quoted))))))
  (is (nil? (error-data '(a/async (fn [] (a/async (a/await :nested-task))))))))

(deftest unsupported-collection-shapes-fail-at-compilation
  (doseq [form ['(c/async (map (fn [x] (a/await x)) []))
                '(c/async (for [x [1]] (a/await x)))
                '(c/async (mapv (fn [x y] (a/await x)) [] []))
                '(c/async (reduce (fn [x y] (a/await x)) []))]]
    (is (= ::c/unsupported-collection (:type (error-data form))))))

(deftest reducible-only-input-is-rejected-before-callbacks
  (let [seen (atom [])
        source (reify clojure.lang.IReduceInit
                 (reduce [_ rf init] (reduce rf init [1 2 3])))
        result (atom nil)]
    (is (= 6 (reduce + 0 source)))
    ((c/async (reduce (fn [acc x]
                        (swap! seen conj x)
                        (a/await (fn [resolve _reject] (resolve (+ acc x)))))
                      0 source))
     #(reset! result %) #(reset! result %))
    (is (= ::c/unsupported-input (:type (ex-data @result))))
    (is (empty? @seen))))

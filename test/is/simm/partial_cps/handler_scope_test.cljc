(ns is.simm.partial-cps.handler-scope-test
  "Lexical exception ownership survives loop trampoline boundaries."
  (:refer-clojure :exclude [await])
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [is.simm.partial-cps.async :refer [async await with-trampoline]]
               :cljs [is.simm.partial-cps.async :refer [await]]))
  #?(:cljs (:require-macros [is.simm.partial-cps.async :refer [async with-trampoline]])))

(defn- immediate [resolve _reject] (resolve :value))
(defn- outcome [op]
  (let [result (atom [])]
    ;; A test runner may itself be inside a CPS callback. Own and drain the
    ;; invocation rather than discarding a Thunk belonging to its trampoline.
    (with-trampoline
      (op #(swap! result conj [:ok %]) #(swap! result conj [:error %])))
    result))

(deftest exceptions-after-a-loop-retain-the-outer-catch-and-finally
  (let [events (atom []) failure (ex-info "after loop" {})
        result (outcome
                (async
                 (try
                   (loop [n 0]
                     (when (< n 2)
                       (try (await immediate)
                            (catch #?(:clj Throwable :cljs :default) _
                              (swap! events conj :wrong-inner-catch)))
                       (recur (inc n))))
                   (throw failure)
                   (catch #?(:clj Throwable :cljs :default) error
                     (swap! events conj [:caught error]) :handled)
                   (finally (swap! events conj :finally)))))]
    (is (= [[:ok :handled]] @result))
    (is (= [[:caught failure] :finally] @events))))

(deftest manually-resumed-loops-keep-the-targets-handler
  (let [events (atom []) resumes (atom []) failure (ex-info "after deferred loop" {})
        suspend (fn [resolve _reject] (swap! resumes conj resolve))
        result (outcome
                (async
                 (try
                   (loop [n 0]
                     (when (< n 2)
                       (try (await suspend)
                            (catch #?(:clj Throwable :cljs :default) _
                              (swap! events conj :wrong-inner-catch)))
                       (recur (inc n))))
                   (throw failure)
                   (catch #?(:clj Throwable :cljs :default) error
                     (swap! events conj [:caught error]) :handled)
                   (finally (swap! events conj :finally)))))]
    (is (= [] @result))
    (is (= 1 (count @resumes)))
    (with-trampoline ((first @resumes) :first))
    (is (= [] @result))
    (is (= 2 (count @resumes)))
    (with-trampoline ((second @resumes) :second))
    (is (= [[:ok :handled]] @result))
    (is (= [[:caught failure] :finally] @events))))

(deftest a-later-iteration-is-not-owned-by-an-earlier-await-catch
  (let [events (atom []) failure (ex-info "next iteration" {})
        result (outcome
                (async
                 (try
                   (loop [n 0]
                     (if (= n 1)
                       (throw failure)
                       (do
                         (try (await immediate)
                              (catch #?(:clj Throwable :cljs :default) _
                                (swap! events conj :wrong-inner-catch)))
                         (recur (inc n)))))
                   (catch #?(:clj Throwable :cljs :default) error
                     (swap! events conj [:caught error]) :handled)
                   (finally (swap! events conj :finally)))))]
    (is (= [[:ok :handled]] @result))
    (is (= [[:caught failure] :finally] @events))))

(deftest a-loop-in-a-catch-propagates-to-the-enclosing-handler
  (let [events (atom []) original (ex-info "original" {}) replacement (ex-info "catch failed" {})
        result (outcome
                (async
                 (try
                   (try
                     (await immediate)
                     (throw original)
                     (catch #?(:clj Throwable :cljs :default) error
                       (swap! events conj [:inner-catch error])
                       (loop [n 0]
                         (when (< n 2) (await immediate) (recur (inc n))))
                       (throw replacement))
                     (finally (swap! events conj :inner-finally)))
                   (catch #?(:clj Throwable :cljs :default) error
                     (swap! events conj [:outer-catch error]) :handled)
                   (finally (swap! events conj :outer-finally)))))]
    (is (= [[:ok :handled]] @result))
    (is (= [[:inner-catch original] :inner-finally [:outer-catch replacement] :outer-finally]
           @events))))

(deftest a-loop-in-finally-replaces-an-error-without-reentering-finally
  (let [events (atom []) original (ex-info "original" {}) replacement (ex-info "finally failed" {})
        result (outcome
                (async
                 (try
                   (try
                     (await immediate)
                     (throw original)
                     (finally
                       (swap! events conj :inner-finally)
                       (loop [n 0]
                         (when (< n 2) (await immediate) (recur (inc n))))
                       (throw replacement)))
                   (catch #?(:clj Throwable :cljs :default) error
                     (swap! events conj [:outer-catch error]) :handled)
                   (finally (swap! events conj :outer-finally)))))]
    (is (= [[:ok :handled]] @result))
    (is (= [:inner-finally [:outer-catch replacement] :outer-finally] @events))))

(deftest deep-recur-stays-stack-safe-and-preserves-rejection
  (let [events (atom []) failure (ex-info "terminal" {})
        result (outcome
                (async
                 (try
                   (loop [n 0]
                     (if (= n 10000)
                       (throw failure)
                       (do (await immediate) (recur (inc n)))))
                   (finally (swap! events conj :finally)))))]
    (is (= 1 (count @result)))
    (is (= :error (ffirst @result)))
    (is (identical? failure (second (first @result))))
    (is (= [:finally] @events))))

(deftest a-deferred-finally-loop-does-not-handle-its-own-replacement-error
  (let [events (atom []) resumes (atom []) original (ex-info "original" {})
        replacement (ex-info "deferred finally failed" {})
        suspend (fn [resolve _reject] (swap! resumes conj resolve))
        result (outcome
                (async
                 (try
                   (try
                     (await immediate)
                     (throw original)
                     (finally
                       (swap! events conj :inner-finally)
                       (loop [n 0]
                         (when (< n 2) (await suspend) (recur (inc n))))
                       (throw replacement)))
                   (catch #?(:clj Throwable :cljs :default) error
                     (swap! events conj [:outer-catch error]) :handled)
                   (finally (swap! events conj :outer-finally)))))]
    (is (= [:inner-finally] @events))
    (is (= [] @result))
    (with-trampoline ((first @resumes) :first))
    (is (= [] @result))
    (with-trampoline ((second @resumes) :second))
    (is (= [[:ok :handled]] @result))
    (is (= [:inner-finally [:outer-catch replacement] :outer-finally] @events))))

(deftest nested-loop-targets-keep-distinct-exception-handlers
  (let [events (atom []) inner (ex-info "inner" {}) outer (ex-info "outer" {})
        result (outcome
                (async
                 (try
                   (loop [n 0]
                     (if (= n 2)
                       (throw outer)
                       (do
                         (try
                           (loop [m 0]
                             (if (= m 2) (throw inner)
                                 (do (await immediate) (recur (inc m)))))
                           (catch #?(:clj Throwable :cljs :default) error
                             (swap! events conj [:inner-catch n error]))
                           (finally (swap! events conj [:inner-finally n])))
                         (recur (inc n)))))
                   (catch #?(:clj Throwable :cljs :default) error
                     (swap! events conj [:outer-catch error]) :handled)
                   (finally (swap! events conj :outer-finally)))))]
    (is (= [[:ok :handled]] @result))
    (is (= [[:inner-catch 0 inner] [:inner-finally 0]
            [:inner-catch 1 inner] [:inner-finally 1]
            [:outer-catch outer] :outer-finally] @events))))

(deftest downstream-code-does-not-reenter-a-completed-inner-try
  (let [events (atom []) failure (ex-info "outside inner scope" {})
        result (outcome
                (async
                 (try
                   (try
                     (loop [n 0]
                       (when (< n 2) (await immediate) (recur (inc n))))
                     (catch #?(:clj Throwable :cljs :default) _
                       (swap! events conj :wrong-inner-catch))
                     (finally (swap! events conj :inner-finally)))
                   (throw failure)
                   (catch #?(:clj Throwable :cljs :default) error
                     (swap! events conj [:outer-catch error]) :handled)
                   (finally (swap! events conj :outer-finally)))))]
    (is (= [[:ok :handled]] @result))
    (is (= [:inner-finally [:outer-catch failure] :outer-finally] @events))))

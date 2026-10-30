(ns is.simm.partial-cps.scope-test
  (:refer-clojure :exclude [await])
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [is.simm.partial-cps.runtime :as rt]
            #?(:clj [is.simm.partial-cps.scope-test-macros :refer [custom-effects]])
            #?(:clj [is.simm.partial-cps.async :refer [async await with-trampoline]]
               :cljs [is.simm.partial-cps.async :refer [await]]))
  #?(:cljs (:require-macros [is.simm.partial-cps.async :refer [async with-trampoline]]
                            [is.simm.partial-cps.scope-test-macros :refer [custom-effects]])))

(def ^:dynamic *context* nil)
(def ^:dynamic *ordinary* :outside)
(defn capture-scope [ctx] (:bindings ctx))
(defn restore-scope [scope current] (assoc current :bindings scope))
(defn restore-exit [saved current entered]
  (if (= (:world saved) (:world entered))
    (assoc current :bindings (:bindings saved)) saved))
(defn immediate [r _] (r :value))
(defn outcome [op]
  (let [values (atom [])]
    (with-trampoline (op #(swap! values conj [:ok %]) #(swap! values conj [:error %])))
    values))

(deftest scope-survives-the-first-inline-trampoline-bounce
  (let [seen (atom [])
        values (binding [*context* {:world :parent :bindings {:key :outer}}]
                 (outcome
                  (async
                   (binding [*context* (assoc *context* :bindings {:key :inner})]
                     (loop [i 0]
                       (when (< i 20)
                         (await immediate)
                         (swap! seen conj *context*)
                         (recur (inc i)))))
                   *context*)))]
    (is (= (vec (repeat 20 {:world :parent :bindings {:key :inner}})) @seen))
    (is (= [[:ok {:world :parent :bindings {:key :outer}}]] @values))))

(deftest repeated-scope-entry-and-exit-has-bounded-stack
  (let [wrong (atom []) depths (atom [])
        values (binding [*context* {:world :parent :bindings {:key :outer}}]
                 (outcome
                  (async
                   (loop [i 0]
                     (if (< i 5000)
                       (do
                         (binding [*context* (assoc *context* :bindings {:key :inner})]
                           (await immediate)
                           (when-not (= {:key :inner} (:bindings *context*))
                             (swap! wrong conj [:inner i])))
                         (when-not (= {:key :outer} (:bindings *context*))
                           (swap! wrong conj [:outer i]))
                         #?(:clj (when (zero? (mod i 500))
                                   (swap! depths conj (count (.getStackTrace (Thread/currentThread))))))
                         (recur (inc i)))
                       :done)))))]
    (is (= [[:ok :done]] @values))
    (is (empty? @wrong))
    #?(:clj (is (< (- (apply max @depths) (apply min @depths)) 20)))))

(deftest a-resumed-scope-follows-the-selected-world
  (let [resumes (atom []) seen (atom [])
        op (async
            (binding [*context* (assoc *context* :bindings {:key :inner})]
              (await (fn [r _] (swap! resumes conj r)))
              (loop [i 0]
                (when (< i 3)
                  (await immediate)
                  (swap! seen conj *context*)
                  (recur (inc i)))))
            *context*)
        values (binding [*context* {:world :parent :bindings {:key :outer}}] (outcome op))]
    (is (empty? @values))
    ;; The embedder supplies both the selected world and the saved scope on
    ;; physical reentry. The compiler must retain that scope on inline bounces.
    (doseq [world [:child :sibling]]
      (binding [*context* {:world world :bindings {:key :runner :leaked true}}]
        (with-trampoline ((first @resumes) :ready))))
    (is (= (vec (concat (repeat 3 {:world :child :bindings {:key :inner}})
                        (repeat 3 {:world :sibling :bindings {:key :inner}}))) @seen))
    (is (= [[:ok {:world :child :bindings {:key :outer}}]
            [:ok {:world :sibling :bindings {:key :outer}}]] @values))))

(deftest registered-scopes-do-not-snapshot-unregistered-vars
  (let [values (binding [*context* {:world :parent :bindings {}}]
                 (outcome (async
                           (binding [*context* (assoc *context* :bindings {:key :inner})
                                     *ordinary* :caller]
                             (await (fn [r _] (set! *ordinary* :handler-change) (r nil)))
                             *ordinary*))))]
    (is (= [[:ok :handler-change]] @values))))

(deftest empty-scope-frames-do-not-wrap-continuations-or-thunks
  (doseq [frame [nil {}]]
    (binding [rt/*scope-frame* frame]
      (let [k (fn [x] x) f (fn [] :value) thunk (rt/->thunk f)]
        (is (identical? k (rt/scope-continuation k)))
        (is (identical? f (.-f thunk)))))))

(deftest scope-capture-happens-after-evaluating-effect-arguments
  (let [values (binding [*context* {:world :parent :bindings {:key :outer}}]
                 (outcome
                  (async
                   (binding [*context* (assoc *context* :bindings {:key :inner})]
                     (await (do (set! *context* (assoc *context* :bindings {:key :argument})) immediate))
                     *context*))))]
    (is (= [[:ok {:world :parent :bindings {:key :argument}}]] @values))))

(deftest a-later-wait-captures-the-restored-outer-scope
  (let [resumes (atom []) seen (atom []) suspend (fn [r _] (swap! resumes conj r))
        op (async
            (binding [*context* (assoc *context* :bindings {:key :inner})]
              (await suspend)
              (swap! seen conj *context*))
            (await suspend)
            (swap! seen conj *context*)
            :done)
        values (binding [*context* {:world :parent :bindings {:key :outer}}] (outcome op))]
    (binding [*context* {:world :child :bindings {:leaked :first}}]
      (with-trampoline ((first @resumes) :first)))
    (binding [*context* {:world :child :bindings {:leaked :second}}]
      (with-trampoline ((second @resumes) :second)))
    (is (= [{:world :child :bindings {:key :inner}}
            {:world :child :bindings {:key :outer}}] @seen))
    (is (= [[:ok :done]] @values))))

(defn syntax-effect [& _] (throw (ex-info "outside CPS" {})))
(defn ignore-effect [& _] (throw (ex-info "outside CPS" {})))
(deftest custom-handlers-retain-argument-syntax-and-evaluation-control
  (is (= [[:ok ['(+ 1 2) :ignored]]]
         @(outcome (custom-effects
                    [(syntax-effect (+ 1 2))
                     (ignore-effect (throw (ex-info "must not evaluate" {})))]))))
  (binding [*context* {:world :parent :bindings {:key :outer}}]
    (is (= [[:ok ['(+ 1 2) :ignored {:key :inner}]]]
           @(outcome (custom-effects
                      (binding [*context* (assoc *context* :bindings {:key :inner})]
                        [(syntax-effect (+ 1 2))
                         (ignore-effect (throw (ex-info "must not evaluate" {})))
                         (:bindings *context*)])))))))

(deftest registered-scope-preserves-parked-error-and-finally-egress
  (let [resume (atom nil) seen (atom [])
        suspend (fn [r _] (reset! resume r) nil)
        op (async
            (try
              (binding [*context* (assoc *context* :bindings {:key :inner})]
                (try
                  (await suspend)
                  (throw (ex-info "expected" {:payload :failure}))
                  (finally
                    (await immediate)
                    (swap! seen conj [:finally *context*]))))
              (catch #?(:clj Throwable :cljs :default) error
                (swap! seen conj [:catch *context* (:payload (ex-data error))])
                :caught)))
        values (binding [*context* {:world :parent :bindings {:key :outer}}]
                 (outcome op))]
    (binding [*context* {:world :child :bindings {:leaked true}}]
      (with-trampoline (@resume :ready)))
    (is (= [[:finally {:world :child :bindings {:key :inner}}]
            [:catch {:world :child :bindings {:key :outer}} :failure]] @seen))
    (is (= [[:ok :caught]] @values))))

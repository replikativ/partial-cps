(ns is.simm.partial-cps.case-dispatch-test
  (:refer-clojure :exclude [await])
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [is.simm.partial-cps.async :refer [await]]
            [is.simm.partial-cps.test-helpers :refer [async-delay]]
            #?(:clj [is.simm.partial-cps.async :refer [async]])
            #?(:clj [is.simm.partial-cps.test-helpers :refer [test-async-fn]]))
  #?(:cljs (:require-macros [is.simm.partial-cps.async :refer [async]]
                            [is.simm.partial-cps.test-helpers :refer [test-async-fn]])))

(def selectors
  [:await :track :external :sleep :spawn :spin :fork :savepoint :scope :custom :custom-helper])

(defn ordinary-case [selector]
  (case selector
    :await :result/await
    :track :result/track
    :external :result/external
    :sleep :result/sleep
    :spawn :result/spawn
    :spin :result/spin
    :fork :result/fork
    :savepoint :result/savepoint
    :scope :result/scope
    :custom :result/custom
    :custom-helper :result/custom-helper
    :result/default))

;; Every branch must contain a registered breakpoint: pure cases bypass inversion.
(defn keyword-case [selector operation]
  (async
   (case selector
     :await (await (operation :await))
     :track (await (operation :track))
     :external (await (operation :external))
     :sleep (await (operation :sleep))
     :spawn (await (operation :spawn))
     :spin (await (operation :spin))
     :fork (await (operation :fork))
     :savepoint (await (operation :savepoint))
     :scope (await (operation :scope))
     :custom (await (operation :custom))
     :custom-helper (await (operation :custom-helper))
     (await (operation :default)))))

(defn keyword-case-no-default [selector operation]
  (async
   (case selector
     :await (await (operation :await))
     :track (await (operation :track))
     :external (await (operation :external))
     :sleep (await (operation :sleep))
     :spawn (await (operation :spawn))
     :spin (await (operation :spin))
     :fork (await (operation :fork))
     :savepoint (await (operation :savepoint))
     :scope (await (operation :scope))
     :custom (await (operation :custom))
     :custom-helper (await (operation :custom-helper)))))

(defn observe [computation]
  (let [completions (atom [])]
    (computation #(swap! completions conj [:resolve %])
                 #(swap! completions conj [:reject %]))
    @completions))

(deftest keyword-dispatch-with-breakpoints
  (doseq [dispatch [keyword-case keyword-case-no-default]
          selector selectors]
    (testing (str selector)
      (let [visited (atom [])
            value (ordinary-case selector)
            outcomes (observe (dispatch selector
                                        (fn [branch]
                                          (fn [resolve _reject]
                                            (swap! visited conj branch)
                                            (resolve (ordinary-case branch))))))]
        (is (= [[:resolve value]] outcomes))
        (is (= [selector] @visited)))))
  (is (= [[:resolve :result/default]]
         (observe (keyword-case :unknown
                                (fn [branch]
                                  (fn [resolve _reject]
                                    (resolve (ordinary-case branch)))))))))

(deftest keyword-dispatch-errors
  (doseq [selector selectors]
    (let [error (ex-info (str selector) {:selector selector})
          visited (atom [])
          outcomes (observe (keyword-case selector
                                          (fn [branch]
                                            (fn [_resolve reject]
                                              (swap! visited conj branch)
                                              (reject error)))))]
      (is (= 1 (count outcomes)))
      (is (= :reject (ffirst outcomes)))
      (is (identical? error (second (first outcomes))))
      (is (= [selector] @visited))))
  (let [visited (atom [])
        outcomes (observe (keyword-case-no-default :unknown
                                                   (fn [branch]
                                                     (swap! visited conj branch))))]
    (is (= 1 (count outcomes)))
    (is (= :reject (ffirst outcomes)))
    (is (re-find #"No matching clause" (ex-message (second (first outcomes)))))
    (is (empty? @visited))))

(deftest integer-dispatch-with-breakpoints
  (doseq [selector (range 12)]
    (let [operation (fn [value] (fn [resolve _reject] (resolve value)))]
      (is (= [[:resolve (case selector
                          0 :zero 1 :one 2 :two 3 :three 4 :four 5 :five
                          6 :six 7 :seven 8 :eight 9 :nine 10 :ten :default)]]
             (observe
              (async
               (case selector
                 0 (await (operation :zero))
                 1 (await (operation :one))
                 2 (await (operation :two))
                 3 (await (operation :three))
                 4 (await (operation :four))
                 5 (await (operation :five))
                 6 (await (operation :six))
                 7 (await (operation :seven))
                 8 (await (operation :eight))
                 9 (await (operation :nine))
                 10 (await (operation :ten))
                 (await (operation :default))))))))))

(deftest throwing-selected-branch-and-control-flow
  (let [error (ex-info "selected branch" {})
        visited (atom [])
        operation (fn [branch]
                    (fn [resolve _reject]
                      (swap! visited conj branch)
                      (resolve branch)))
        computation (fn []
                      (async
                       (case :custom-helper
                         :await (await (operation :await))
                         :track (await (operation :track))
                         :external (await (operation :external))
                         :sleep (await (operation :sleep))
                         :spawn (await (operation :spawn))
                         :spin (await (operation :spin))
                         :fork (await (operation :fork))
                         :savepoint (await (operation :savepoint))
                         :scope (await (operation :scope))
                         :custom (await (operation :custom))
                         :custom-helper (do (await (operation :custom-helper))
                                            (throw error)))))]
    (let [outcomes (observe (computation))]
      (is (= 1 (count outcomes)))
      (is (= :reject (ffirst outcomes)))
      (is (identical? error (second (first outcomes)))))
    (is (= [:custom-helper] @visited))
    (reset! visited [])
    (is (= [[:resolve error]]
           (observe (async
                     (try
                       (await (computation))
                       (catch #?(:clj Exception :cljs :default) caught
                         caught)
                       (finally
                         (swap! visited conj :finally)))))))
    (is (= [:custom-helper :finally] @visited))))

(deftest deferred-dispatch-in-loop
  (let [visited (atom [])
        operation (fn [branch]
                    (swap! visited conj branch)
                    (async-delay 1 (ordinary-case branch)))]
    (test-async-fn
     (async
      (loop [remaining selectors results []]
        (if (seq remaining)
          (let [selector (await (async-delay 1 (first remaining)))
                value (await (keyword-case selector operation))]
            (recur (rest remaining) (conj results value)))
          results)))
     (fn [results]
       (is (= (mapv ordinary-case selectors) results))
       (is (= selectors @visited))))))

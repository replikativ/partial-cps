(ns is.simm.partial-cps.binding-capture-test
  (:refer-clojure :exclude [await])
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [is.simm.partial-cps.runtime :as rt]
            #?(:clj [is.simm.partial-cps.binding-capture-test-macros])
            #?(:clj [is.simm.partial-cps.async :refer [async await with-trampoline]]
               :cljs [is.simm.partial-cps.async :refer [await]]))
  #?(:cljs (:require-macros [is.simm.partial-cps.async :refer [async with-trampoline]]
                            [is.simm.partial-cps.binding-capture-test-macros])))

(def ^:dynamic *context* nil)
(def ^:dynamic *raw* nil)
(def ^:dynamic *captured-only* nil)
(def ^:dynamic *events* nil)
(def ^:dynamic *entry-data* nil)
(def ^:dynamic *capture-action* nil)

(defn capture-entry [saved]
  (let [data @*entry-data*]
    (swap! *events* conj [:capture saved data])
    (when (= data :fail)
      (throw (ex-info "entry capture failed" {:phase :capture})))
    (when *capture-action* (*capture-action*))
    {:saved saved :entry data}))

(defn restore-entry [captured current entered]
  (swap! *events* conj [:restore captured current entered])
  (:saved captured))

(defn restore-raw [saved current entered]
  (swap! *events* conj [:raw saved current entered])
  saved)

(defn capture-scope [value]
  (swap! *events* conj [:scope (:scope value)])
  (:scope value))

(defn restore-scope [scope current]
  (assoc current :scope scope))

(defn immediate [r _] (r :ready))

(defn outcome [op]
  (let [values (atom [])]
    (with-trampoline
      (op #(swap! values conj [:ok %]) #(swap! values conj [:error %])))
    values))

(defn events-of [kind events]
  (filterv #(= kind (first %)) @events))

(deftest entry-capture-precedes-all-initializers-and-uses-all-raw-saves
  (let [events (atom []) entry-data (atom :entry) raw {:raw :outer}
        outer {:scope :outer}]
    (binding [*events* events *entry-data* entry-data
              *context* outer *raw* raw
              *capture-action* (fn [] (set! *raw* :capture-change))]
      (let [values (outcome
                    (async
                     (binding [*raw* (do
                                       (swap! events conj [:init *raw*])
                                       (reset! entry-data :initializer-change)
                                       :inner)
                               *context* (do
                                           (swap! events conj [:init *context*])
                                           {:scope :inner})]
                       (await immediate))
                     [*context* *raw*]))]
        (is (= [[:capture outer :entry]
                [:init :capture-change]
                [:init outer]]
               (subvec @events 0 3)))
        (is (= [[:ok [outer raw]]] @values))
        (is (= [[:raw raw :inner :inner]] (events-of :raw events)))
        (is (= [[:restore {:saved outer :entry :entry}
                 {:scope :inner} {:scope :inner}]]
               (events-of :restore events)))
        (is (= 1 (count (events-of :capture events))))))))

(deftest entry-data-survives-parking-nested-exit-and-trampoline-bounces
  (doseq [completion [:success :error]]
    (let [events (atom []) entry-data (atom :entry) parked (atom [])
          seen (atom []) outer {:scope :outer}
          suspend (fn [r e] (swap! parked conj [r e]) nil)]
      (binding [*events* events *entry-data* entry-data *context* outer]
        (let [values
              (outcome
               (async
                (try
                  (binding [*context* {:scope :inner}]
                    (binding [*context* {:scope :deep}]
                      (try
                        (await suspend)
                        (loop [i 0]
                          (when (< i 4)
                            (await immediate)
                            (swap! seen conj *context*)
                            (recur (inc i))))
                        (await suspend)
                        (finally
                          (await immediate)
                          (swap! seen conj [:finally *context*]))))
                    (swap! seen conj [:nested-exit *context*]))
                  [:success *context*]
                  (catch #?(:clj Throwable :cljs :default) error
                    [:error *context* (:reason (ex-data error))]))))]
          (is (empty? @values))
          (is (= [[:capture outer :entry]
                  [:capture {:scope :inner} :entry]]
                 (events-of :capture events)))
          (reset! entry-data :resume)
          (binding [*context* {:scope :runner}]
            (with-trampoline ((first (first @parked)) :first)))
          (is (= 2 (count @parked)))
          (binding [*context* {:scope :another-runner}]
            (with-trampoline
              (if (= completion :success)
                ((first (second @parked)) :second)
                ((second (second @parked)) (ex-info "expected" {:reason :failure})))))
          (is (= [[:ok (if (= completion :success)
                         [:success outer]
                         [:error outer :failure])]] @values))
          (is (= (vec (concat (repeat 4 {:scope :deep})
                              [[:finally {:scope :deep}]]
                              (when (= completion :success)
                                [[:nested-exit {:scope :inner}]])))
                 @seen))
          (is (= 2 (count (events-of :capture events))))
          (is (= [{:saved {:scope :inner} :entry :entry}
                  {:saved outer :entry :entry}]
                 (mapv second (events-of :restore events))))
          ;; Scope descriptors capture the entered scope at effects and bounces;
          ;; the independent entry hook captures each saved outer value once.
          (is (< 2 (count (events-of :scope events))))
          (is (every? #{:deep :inner} (map second (events-of :scope events)))))))))

(deftest capture-failure-exits-before-initializers-or-body-and-leaves-bindings-balanced
  (let [events (atom []) entry-data (atom :fail) outer {:scope :outer}
        op (async
            (binding [*context* (do (swap! events conj [:init]) {:scope :inner})
                      *raw* (do (swap! events conj [:raw-init]) :inner)]
              (swap! events conj [:body])
              (await immediate))
            *context*)]
    (binding [*events* events *entry-data* entry-data *context* outer *raw* :outer]
      (let [frame rt/*scope-frame*
            values (outcome op)]
        (is (= [:error] (mapv first @values)))
        (is (= {:phase :capture} (ex-data (second (first @values)))))
        (is (= [[:capture outer :fail]] @events))
        (is (= outer *context*))
        (is (= :outer *raw*))
        (is (identical? frame rt/*scope-frame*))
        (reset! entry-data :retry)
        (is (= [[:ok outer]] @(outcome op)))
        (is (= outer *context*))
        (is (= :outer *raw*))))))

(deftest two-argument-registration-passes-the-original-value-on-later-resumption
  (let [events (atom []) saved {:raw :outer} resume (atom nil)
        op (async
            (binding [*raw* :entered]
              (await (fn [r _] (reset! resume r) nil)))
            *raw*)]
    (binding [*events* events *raw* saved]
      (let [values (outcome op)]
        (is (empty? @events))
        (binding [*raw* :runner]
          (with-trampoline (@resume :ready)))
        (is (= [[:raw saved :runner :entered]] @events))
        (is (identical? saved (second (first @events))))
        (is (= [[:ok saved]] @values))))))

(deftest entry-capture-works-without-a-binding-scope-descriptor
  (let [events (atom []) entry-data (atom :entry) outer {:scope :outer}
        inner {:scope :inner}]
    (binding [*events* events *entry-data* entry-data *captured-only* outer]
      (let [values (outcome
                    (async
                     (loop [i 0 seen []]
                       (if (= i 4)
                         [seen *captured-only*]
                         (recur (inc i)
                                (conj seen
                                      (binding [*captured-only* inner]
                                        (await immediate)
                                        *captured-only*)))))))]
        (is (= [[:ok [(vec (repeat 4 inner)) outer]]] @values))
        (is (= (vec (repeat 4 [:capture outer :entry]))
               (events-of :capture events)))
        (is (= (vec (repeat 4 [:restore {:saved outer :entry :entry} inner inner]))
               (events-of :restore events)))
        (is (empty? (events-of :scope events)))))))

(deftest binding-without-breakpoints-retains-ordinary-binding-semantics
  (let [events (atom []) entry-data (atom :fail) outer {:scope :outer}]
    (binding [*events* events *entry-data* entry-data *context* outer]
      (let [values (outcome
                    (async
                     (let [result (binding [*context* (do
                                                        (swap! events conj [:init *context*])
                                                        {:scope :inner})]
                                    (swap! events conj [:body *context*])
                                    (set! *context* {:scope :body-change})
                                    *context*)]
                       (await immediate)
                       [result *context*])))]
        (is (= [[:ok [{:scope :body-change} outer]]] @values))
        (is (= [[:init outer] [:body {:scope :inner}]] @events))
        (is (zero? (count (events-of :capture events))))
        (is (empty? (events-of :restore events)))
        (is (= outer *context*))))))

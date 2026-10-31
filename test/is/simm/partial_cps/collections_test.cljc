(ns is.simm.partial-cps.collections-test
  (:refer-clojure :exclude [await])
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :as test :refer-macros [deftest is]])
            [is.simm.partial-cps.async :as a :refer [await]]
            [is.simm.partial-cps.collections :as c]
            #?(:clj [is.simm.partial-cps.collection-macros :refer [hidden-await wrapped-mapv]]))
  #?(:cljs (:require-macros [is.simm.partial-cps.collection-macros :refer [hidden-await wrapped-mapv rejection]])))

(defn ready [x] (fn [resolve _reject] (resolve x)))

(defn immediate [task]
  (let [result (atom ::pending)]
    (task #(reset! result %) #(reset! result %))
    @result))

(deftest eager-operations-and-macro-generated-calls
  (is (= [[2 4 6] [1 3] 6 [2 3]]
         (immediate
          (c/async
           [(mapv (fn [x] (await (ready (* 2 x)))) [1 2 3])
            (filterv (fn [x] (odd? (await (ready x)))) [0 1 2 3])
            (reduce (fn [acc x] (+ acc (await (ready x)))) 0 [1 2 3])
            (wrapped-mapv (fn [x] (hidden-await (ready (inc x)))) [1 2])])))))

(deftest termination-and-evaluation-order
  (let [seen (atom [])]
    (is (nil? (immediate
               (c/async
                (run! (fn [x]
                        (swap! seen conj (await (ready x)))
                        (when (= x 2) (reduced :stop))) [1 2 3])))))
    (is (= [1 2] @seen)))
  (let [seen (atom [])]
    (is (= 3 (immediate
              (c/async
               (reduce (fn [acc x]
                         (swap! seen conj x)
                         (let [value (+ acc (await (ready x)))]
                           (if (= x 2) (reduced value) value))) 0 [1 2 3])))))
    (is (= [1 2] @seen)))
  (is (= [0 0] (immediate
                (c/async
                 (mapv (fn [x]
                         (let [v (await (ready x))]
                           (if (pos? v) (recur (dec v)) v))) [2 3]))))))

(deftest multi-shot-accumulators-are-independent
  (let [parked (atom nil)
        results (atom [])
        source (fn [x] (fn [resolve _reject]
                         (if (= x 2) (reset! parked resolve) (resolve x))))]
    ((c/async (mapv (fn [x] (await (source x))) [1 2 3]))
     #(swap! results conj %) #(swap! results conj %))
    (is (empty? @results))
    (@parked 20)
    (@parked 200)
    (is (= [[1 20 3] [1 200 3]] @results))))

(deftest core-doseq-and-pure-laziness
  (is (= [[0 0] [0 2] [1 0] [1 2]]
         (immediate
          (c/async
           (let [seen (atom [])]
             (doseq [x [0 1] y (range 5) :let [v y] :when (even? v) :while (< v 3)]
               (swap! seen conj (await (ready [x y]))))
             @seen)))))
  (is (= [4]
         (immediate
          (c/async
           (let [seen (atom [])]
             (doseq [x (await (ready [1 2 3 4]))
                     :let [v (await (ready (* 2 x)))]
                     :when (await (ready (even? x)))
                     :while (await (ready (< x 4)))]
               (swap! seen conj (await (ready v))))
             @seen)))))
  (is (= '(1 2 3) (immediate (c/async (take 3 (map inc (range)))))))
  (is (= :custom
         (immediate (c/async
                     (let [mapv (fn [_f _xs] :custom)]
                       (mapv inc [1 2]))))))
  (is (= [1 2]
         (mapv immediate (immediate (c/async
                                     (mapv (fn [x] (a/async (await (ready x)))) [1 2])))))))

(deftest deferred-callback-completion
  (let [later (fn [x]
                (fn [resolve _reject]
                  #?(:clj (future (Thread/sleep 1) (resolve x))
                     :cljs (js/setTimeout #(resolve x) 1))))
        task (c/async (mapv (fn [x] (await (later x))) [1 2 3]))]
    #?(:clj
       (let [result (promise)]
         (task #(deliver result %) #(deliver result %))
         (is (= [1 2 3] (deref result 3000 ::timeout))))
       :cljs
       (test/async done
                   (task (fn [value] (is (= [1 2 3] value)) (done))
                         (fn [error] (is false (str error)) (done)))))))

#?(:cljs
   (deftest closure-diagnostics-on-clojurescript
     (is (= :is.simm.partial-cps.ioc/unsupported-suspension
            (rejection (mapv (fn [x] (await x)) []))))
     (is (= :is.simm.partial-cps.ioc/unsupported-suspension
            (rejection (mapv (fn [x] (hidden-await x)) []))))))

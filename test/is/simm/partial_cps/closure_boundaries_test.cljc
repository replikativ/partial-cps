(ns is.simm.partial-cps.closure-boundaries-test
  (:refer-clojure :exclude [await])
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [is.simm.partial-cps.async :as a :refer [await]]
            #?(:clj [is.simm.partial-cps.boundary-macros :refer [hidden-await]]))
  #?(:cljs (:require-macros [is.simm.partial-cps.boundary-macros :refer [hidden-await rejection]])))

(defn ready [x] (fn [resolve _reject] (resolve x)))

(defn immediate [task]
  (let [result (atom ::pending)]
    (task #(reset! result %) #(reset! result %))
    @result))

(deftest core-doseq-and-pure-laziness
  (is (= [[0 0] [0 2] [1 0] [1 2]]
         (immediate
          (a/async
           (let [seen (atom [])]
             (doseq [x [0 1] y (range 5) :let [v y] :when (even? v) :while (< v 3)]
               (swap! seen conj (await (ready [x y]))))
             @seen)))))
  (is (= [4]
         (immediate
          (a/async
           (let [seen (atom [])]
             (doseq [x (await (ready [1 2 3 4]))
                     :let [v (await (ready (* 2 x)))]
                     :when (await (ready (even? x)))
                     :while (await (ready (< x 4)))]
               (swap! seen conj (await (ready v))))
             @seen)))))
  (is (= '(1 2 3) (immediate (a/async (take 3 (map inc (range)))))))
  (is (= :custom
         (immediate (a/async
                     (let [mapv (fn [_f _xs] :custom)]
                       (mapv inc [1 2]))))))
  (is (= [1 2]
         (mapv immediate (immediate (a/async
                                     (mapv (fn [x] (a/async (await (ready x)))) [1 2])))))))

(deftest pure-callback-bindings-and-quoted-data
  (is (= [2] (vec (immediate (a/async (map (fn [await] (await 1)) [inc]))))))
  (is (= ['(a/await x)]
         (vec (immediate (a/async (map (fn [x] '(a/await x)) [1])))))))

#?(:cljs
   (deftest closure-diagnostics-on-clojurescript
     (is (= :is.simm.partial-cps.ioc/unsupported-suspension
            (rejection (mapv (fn [x] (await x)) []))))
     (is (= :is.simm.partial-cps.ioc/unsupported-suspension
            (rejection (mapv (fn [x] (hidden-await x)) []))))))

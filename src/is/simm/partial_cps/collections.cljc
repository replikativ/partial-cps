(ns is.simm.partial-cps.collections
  "Optional sequential collection lowering for CPS compilers.
  Eager operations accept seqable inputs and inline visible effectful function
  literals. Lazy operations are rejected unless the embedding compiler selects
  :collection-mode :legacy-eager. Accumulators are persistent, not transient."
  (:require [is.simm.partial-cps.async]
            #?(:clj [is.simm.partial-cps.ioc :as ioc]))
  #?(:cljs (:require-macros [is.simm.partial-cps.collections :refer [async]])))

(defn input-seq
  "Validate the explicit seqable-input contract before executing callbacks."
  [operation coll]
  (when-not (seqable? coll)
    (throw (ex-info (str "CPS " operation " requires a seqable input; native "
                         "reduction protocols cannot suspend their callbacks. "
                         "Supply a sequence or an explicit CPS source adapter.")
                    {:type ::unsupported-input :operation operation
                     :input-type (str (type coll))})))
  (seq coll))

#?(:clj
   (do
     (def ^:private eager #{"mapv" "filterv" "reduce" "run!"})
     (def ^:private lazy-ops #{"map" "filter" "remove" "keep" "for"})

     (defn- core-name [ctx sym]
       (when (symbol? sym)
         (let [v (ioc/var-name (:env ctx) sym)]
           (when (and v (#{"clojure.core" "cljs.core"} (namespace v))) (name v)))))

     (defn- occurs? [sym body]
       (some #{sym} (tree-seq coll? seq body)))

     (defn- literal [f]
       (when (and (seq? f) (#{'fn 'fn* 'clojure.core/fn 'cljs.core/fn} (first f)))
         (let [[_ & tail] f
               self (when (symbol? (first tail)) (first tail))
               tail (if self (rest tail) tail)
               [params & body] tail]
           (when (and (vector? params) (not (some #{'&} params))
                      (not (and self (occurs? self body)))
                      (not (and (map? (first body))
                                (some #(contains? (first body) %) [:pre :post]))))
             [params body]))))

     (defn- unsupported! [form operation reason]
       (throw (ex-info
               (str "Unsupported effectful " operation ": " reason
                    ". Use doseq/loop or an explicit asynchronous sequence "
                    "comprehension; lazy operations are not made eager by default.")
               {:type ::unsupported-collection :operation operation :reason reason
                :form form :file *file* :line (:line (meta form))
                :column (:column (meta form))})))

     (defn- inline-call [bindings body]
       (if (occurs? 'recur body) `(loop ~bindings ~@body) `(let ~bindings ~@body)))

     (defn- seq-loop [operation coll initial step stop?]
       (let [s (gensym "items") acc (gensym "acc") x (gensym "item") v (gensym "value")]
         `(loop [~acc ~initial ~s (input-seq ~operation ~coll)]
            (if ~s
              (let [~x (first ~s) ~v ~(step x acc)]
                ~(if stop?
                   `(if (reduced? ~v) (deref ~v) (recur ~v (next ~s)))
                   `(recur ~v (next ~s))))
              ~acc))))

     (defn rewrite
       "A compiler's :form-rewriter hook. Default mode supports eager operations
       only; :collection-mode :legacy-eager opts into the former Spindel vector
       semantics for lazy operations. doseq always uses the core macro expansion.
       The hook runs again after macro expansion, respecting the compiler env."
       [ctx form]
       (let [[op & args] (when (seq? form) form)
             operation (core-name ctx op)
             legacy? (= :legacy-eager (:collection-mode ctx))]
         (or
          (when (or (eager operation) (lazy-ops operation))
            (if (= operation "for")
              (let [[bindings & body] args]
                (when (ioc/has-breakpoints? (cons 'do body) ctx)
                  (when-not legacy? (unsupported! form operation :lazy-suspension))
                  (when-not (and (vector? bindings) (= 2 (count bindings)))
                    (unsupported! form operation :requires-one-binding-pair))
                  (let [[binding coll] bindings]
                    (seq-loop operation coll []
                              (fn [x acc] `(conj ~acc (let [~binding ~x] ~@body))) false))))
              (let [[f & rest-args] args
                    [params body] (literal f)]
                (when (and (seq? f) (ioc/has-breakpoints? f ctx))
                  (when (and (lazy-ops operation) (not legacy?))
                    (unsupported! form operation :lazy-suspension))
                  (when-not params (unsupported! form operation :requires-simple-function-literal))
                  (if (= operation "reduce")
                    (do
                      (when-not (and (= 2 (count rest-args)) (= 2 (count params)))
                        (unsupported! form operation :requires-initial-value-and-binary-function))
                      (let [[initial coll] rest-args [a p] params]
                        (seq-loop operation coll initial
                                  (fn [x acc] (inline-call [a acc p x] body)) true)))
                    (do
                      (when-not (and (= 1 (count rest-args)) (= 1 (count params)))
                        (unsupported! form operation :requires-unary-function-and-one-collection))
                      (let [[p] params coll (first rest-args)
                            call (fn [x] (inline-call [p x] body))]
                        (case operation
                          ("map" "mapv")
                          (seq-loop operation coll [] (fn [x acc] `(conj ~acc ~(call x))) false)
                          ("filter" "filterv")
                          (seq-loop operation coll [] (fn [x acc] `(if ~(call x) (conj ~acc ~x) ~acc)) false)
                          "remove"
                          (seq-loop operation coll [] (fn [x acc] `(if ~(call x) ~acc (conj ~acc ~x))) false)
                          "keep"
                          (seq-loop operation coll [] (fn [x acc] `(let [v# ~(call x)]
                                                                     (if (nil? v#) ~acc (conj ~acc v#)))) false)
                          "run!"
                          `(do ~(seq-loop operation coll nil (fn [x _] (call x)) true) nil)))))))))
          form)))

     (defmacro ^{:partial-cps/scope true} async
       "Like partial-cps.async/async, with optional eager collection lowering.
       Lazy core operations retain their contract and reject bare suspension."
       [& body]
       (binding [ioc/*form-rewriter* rewrite]
         (apply @#'is.simm.partial-cps.async/async &form &env body)))))

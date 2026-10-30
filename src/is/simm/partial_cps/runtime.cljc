(ns is.simm.partial-cps.runtime
  (:refer-clojure :exclude [bound-fn]))

(defn ^:no-doc bound-fn
  [f]
  #?(:clj
     ;; Delegate to clojure.core/bound-fn* (identical frame-capture
     ;; semantics on the JVM; note bound-fn without the * is a macro taking
     ;; an fntail, not a fn value) instead of hand-rolling
     ;; Var/getThreadBindingFrame statics, which alternative runtimes
     ;; (e.g. Jolt) do not expose.
     (clojure.core/bound-fn* f)
     ;; no dynamic binding support for async code in cljs (same for core.async)
     :cljs identity))

(def ^:dynamic ^:no-doc *scope-frame*
  "Embedder-provided lexical scope descriptors; never a thread binding frame."
  nil)

(defn- capture-scope [frame]
  (mapv (fn [[_ [capture restore bind-value]]]
          [restore bind-value (capture)]) frame))

(defn- call-in-scope [frame captured f]
  ;; Resolve every value in the runner's context before pushing bindings.
  (let [values (mapv (fn [[restore bind-value value]] [bind-value (restore value)]) captured)]
    (binding [*scope-frame* frame]
      (letfn [(run [i]
                (if (< i (count values))
                  (let [[bind-value value] (nth values i)]
                    (bind-value value #(run (inc i))))
                  (f)))]
        (run 0)))))

(defn ^:no-doc scope-continuation
  "Capture registered embedder scope data for a continuation. The runner
  supplies execution authority; adapters restore only their lexical scope."
  [k]
  (if (seq *scope-frame*)
    (let [frame *scope-frame* captured (capture-scope frame)]
      (fn [value] (call-in-scope frame captured #(k value))))
    k))

(defn- scope-thunk [f]
  (if (seq *scope-frame*)
    (let [frame *scope-frame* captured (capture-scope frame)]
      (fn [] (call-in-scope frame captured f)))
    f))

(deftype Thunk [f])

(defn ->thunk
  "Create a thunk for trampolining"
  [f]
  (Thunk. (scope-thunk f)))

(defn thunk?
  "Whether `x` is a trampoline Thunk.

   Emitted CPS code (the `async` macro + await-handler) MUST call this fn rather
   than inline `(instance? Thunk x)`: a fn is referenced by an ns-qualified VAR
   (`is.simm.partial-cps.runtime/thunk?`), which resolves under any macro
   re-scoping. The dotted TYPE symbol `is.simm.partial_cps.runtime.Thunk` is a
   property-access form on cljs that some macros (e.g. `cljs.test/async`)
   re-scope to `cljs.test.is.simm…` → `undefined` at runtime. spindel hit exactly
   this and worked around it locally (seq/core.cljc); centralizing it here lets
   that workaround go away."
  [x]
  (instance? Thunk x))

(defn force-thunk
  "Run a Thunk's deferred fn — one trampoline bounce. Companion to `thunk?`;
   keeps the `Thunk` type reference inside this namespace (see `thunk?`)."
  [t]
  ((.-f ^Thunk t)))

;; -----------------------------------------------------------------------------
;; Which trampoline is running here?
;;
;; Host-specific, so it lives here: `async.cljc` is also interpreted inside
;; sandboxes (SCI with interop locked) that inject this namespace natively.
;;
;; `async/*in-trampoline*` alone cannot answer the question. It is a dynamic
;; var, and dynamic bindings travel: `future`, agents, `bound-fn` and
;; core.async go blocks restore the bindings of whoever created them, on
;; another thread or LATER ON THE SAME ONE, when the trampoline that bound the
;; flag has long unwound. A continuation that believes such a flag returns its
;; Thunk to a caller that is no trampoline, and is lost. So every activation
;; has its own token; the var carries it (and travels), this thread-local
;; carries it too (and does not), and a trampoline is running here exactly
;; when the two agree.
;; -----------------------------------------------------------------------------

(def ^:private active-trampoline
  #?(:clj (ThreadLocal.) :cljs (volatile! nil)))

(defn enter-trampoline!
  "Mark a new trampoline activation on this thread. Returns [token previous]:
   bind `async/*in-trampoline*` to `token`, pass `previous` to
   `leave-trampoline!` in a `finally`."
  []
  (let [token #?(:clj (Object.) :cljs (js-obj))
        previous #?(:clj (.get ^ThreadLocal active-trampoline) :cljs @active-trampoline)]
    #?(:clj (.set ^ThreadLocal active-trampoline token)
       :cljs (vreset! active-trampoline token))
    [token previous]))

(defn leave-trampoline!
  [previous]
  #?(:clj (if (nil? previous)
            (.remove ^ThreadLocal active-trampoline)
            (.set ^ThreadLocal active-trampoline previous))
     :cljs (vreset! active-trampoline previous))
  nil)

(defn owns-trampoline?
  "Whether `token`, the current value of `async/*in-trampoline*`, is the
   trampoline activation that is running on this thread right now."
  [token]
  (and (some? token)
       (identical? token #?(:clj (.get ^ThreadLocal active-trampoline)
                            :cljs @active-trampoline))))

(defn call-in-trampoline
  "Run `(f token)` as a new trampoline activation on this thread: `token` is
   the activation's, for `f` to bind as `async/*in-trampoline*` while it
   forces Thunks. The activation is left however `f` ends.

   An interpreter running untrusted code can expose this instead of
   `enter-trampoline!` and `leave-trampoline!`: the code then cannot leave an
   activation unbalanced, nor learn the token of another, and so cannot make a
   continuation that carries a stale one return its Thunk to no trampoline."
  [f]
  (let [[token previous] (enter-trampoline!)]
    (try
      (f token)
      (finally (leave-trampoline! previous)))))

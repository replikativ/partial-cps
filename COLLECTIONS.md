# Collections and suspension boundaries

Ordinary `async` preserves Clojure's collection semantics. A bare breakpoint
inside a function, lazy-sequence body, or method cannot suspend its caller and
is rejected during macro expansion, including sites introduced by macros.
The error includes the breakpoint, offending site, enclosing form, and source
location where available. Unknown effects inside separately compiled functions
cannot be diagnosed statically; runtime breakpoint shims still enforce the
boundary. Quoted data and locally shadowed names are not suspension sites.

`doseq` needs no special lowering. Its native macro expansion handles nested
bindings, destructuring and modifiers in a resumable scope.

## Optional eager operations

```clojure
(require '[is.simm.partial-cps.async :refer [await]]
         '[is.simm.partial-cps.collections :as collections])

(collections/async
  (mapv (fn [x] (await (fetch x))) inputs))
```

`collections/async` supports sequential `mapv`, `filterv`, `run!`, and `reduce`
with an explicit initial value. Only visible, single-arity, non-variadic
function literals are inlined: unary for the first three operations, binary
for `reduce`. Destructuring and callback `recur` are supported. Self-referencing
named functions, pre/post conditions, multiple collections and transducer
arities are rejected when they contain suspension. Pure calls are unchanged.
The pass also handles eligible calls introduced by user macros.

Inputs must be seqable. This is an explicit restriction relative to Clojure's
native reduction protocols: a host `IReduceInit` implementation cannot suspend
its synchronous callback. Unsupported inputs fail before callback execution
with `:is.simm.partial-cps.collections/unsupported-input`. Adapt the source to
a sequence or provide an explicit asynchronous source adapter.

Both `reduce` and `run!` respect `reduced`. Accumulators use persistent values,
so resuming a copied continuation does not reuse a mutable transient builder.
This does not copy arbitrary mutable inputs or user callback state.

## Lazy operations

Effectful core `map`, `filter`, `remove`, `keep` and `for` are not silently made
eager by `collections/async`. Use `doseq`/`loop` to execute effects, or explicitly
consume `is.simm.partial-cps.sequence/for` and `PAsyncSeq` for asynchronous lazy
pulling. These are distinct from Clojure's memoized `LazySeq` contract.
Their mutable memoization and source adapters require a separate review before
use across execution-world forks; this change makes no new fork-safety promise.

## Embedding compilers

An embedding can pass these keys to `ioc/invert`:

```clojure
{:form-rewriter is.simm.partial-cps.collections/rewrite
 :collection-mode :legacy-eager
 :opaque-scopes #{'my.runtime/spin}}
```

`:form-rewriter` receives `[ctx form]` at each transformed expression, including
after macro expansion, with the compiler's current lexical environment. The
optional dynamic `ioc/*form-rewriter*` is a macro-time convenience used by
`collections/async`. Do not enable a rewriter globally by requiring a namespace.

`:legacy-eager` is an explicit compatibility option for existing embeddings.
It additionally lowers effectful lazy operations to vectors, with one input
collection and (for `for`) one binding pair. Unsupported shapes fail at compile
time. It deliberately does not rewrite `doseq`. Removing or changing this
compatibility policy is a separate migration.

An own-scope macro may instead carry `:partial-cps/scope true` metadata, as
`async` does. This declares that the macro checks and transforms its own body;
it is not an escape hatch for returning an untransformed effectful function.
Constructing tasks in an ordinary callback is valid, but a CPS-aware consumer
must still execute/await those tasks.

The tests cover both synchronous and genuinely deferred callback completion,
macro-hidden rejection, collection macros, reduced termination, native doseq,
pure laziness, and multiple resumes of a captured continuation.

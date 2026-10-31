# Suspension boundaries

`async` transforms effects within its execution scope. Function literals,
methods, and lazy-sequence bodies establish separate function boundaries: a
bare breakpoint there cannot suspend the enclosing caller. Such visible sites
are rejected during macro expansion, including sites introduced by macros.
The diagnostic includes the breakpoint, offending site, enclosing form, and
source location where available. Quoted data and locally shadowed names are
not suspension sites. Effects hidden inside separately compiled functions
remain a runtime boundary; this is not whole-program effect analysis.

Core `doseq` and `loop` work through their native expansions. `doseq` supports
nested bindings, destructuring, and `:let`, `:when`, and `:while` modifiers,
including effects in their expressions. Higher-order functions retain their
ordinary callback contracts; callbacks are not implicitly inlined or made
resumable. Lazy `map` and core `for` retain their native lazy semantics.

Give an asynchronous callback its own `async` scope and consume its returned
task through an explicit CPS-aware caller. For lazy asynchronous processing,
use `is.simm.partial-cps.sequence/for` and `PAsyncSeq`. Their pulling contract is
distinct from Clojure's memoized `LazySeq`; shared mutable sequence caches are
not made fork-safe by a continuation transform.

## Embedding compilers

Mark macros which establish their own CPS scope with
`:partial-cps/scope true` metadata, as `async` does. An embedding compiler can
also pass `:opaque-scopes #{fully.qualified/scope-macro}` to `ioc/invert`.
The enclosing compiler then leaves those bodies to their own transformation.
This does not make arbitrary callback consumers CPS-aware.

The JVM class-file name limitation and function-splitting workaround are
covered in the [README](README.md#jvm-class-file-name-limit).

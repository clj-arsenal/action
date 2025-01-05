Actions provide a data-oriented way to manipulate your system
state in a controlled and highly observable way.

An action is a persistent collection of effects, and
(optionally) a map of headers.

```clojure
(require '[clj-arsenal.action :refer [act << dispatcher] :as action])

(act {:key ::my-action}
 [:http (<< :http/post "api/example" :json {:foo 1 :bar 2})]
 [:patch {:path [:foo] :change [:value (<< :http/response :get :foo)]}])

; -> #clj-arsenal.action/Action{:headers {:key ::my-action} :effects ...}
; it's just a record
```

When dispatched, an action's effects are executed in order;
with their injections (the `<<` forms) within each effect
being resolved before execution.

A dispatcher is a function which takes an action, wraps it
in a context map, and runs said context through a series
of interceptors.  A minimal dispatcher can be created like
this:

```clojure
(defn executor
  [context [effect-kind & args :as effect]]
  ;; execute effect
  context)

(defn injector
  [context {:keys [kind args] :as injection}]
  ;; return injected value
  )

(defn context-builder
  [context-basis & args]
  ;; return a context map
  context-basis)

(def dispatch!
  (dispatcher [(action/errors) (action/effects executor injector)]
    :context-builder context-builder))

(dispatch!
  (act {:key ::my-action}
    [:http (<< :http/post "api/example" :json {:foo 1 :bar 2})]
    [:patch {:path [:foo] :change [:value (<< :http/response :get :foo)]}])
  ...extra-args)
```

Here's what happens when an action is dispatched:

1. The `context-builder` is called to create an initial dispatch context.
   It's passed a minimal `context-stub` map and any extra arguments passed
   to `dispatch!`.  The `context-stub` is a minimal context, which can be
   modified as needed, or returned unmodified.  The `context-builder` is
   optional, if omitted then the minimal `context-stub` will be used as
   the initial dispatch context.
2. The dispatch context is passed through the provided interceptor chain,
   in this case the errors and effects interceptors.  The errors interceptor
   does nothing on entry, and logs any errors on leave.  The effects interceptor
   executes the dispatched action's effects.  For each effect, it resolves
   all injections through `injector`, then executes the effect with `executor`.
   If either `injector` or `executor` return a chainable (i.e async) value, then
   this too will be resolved before continuing the dispatch.

The `executor` returns a new (or the same) context map, or a chainable that
resolves to a context map.

The `injector` returns the value to be injected, or a chainable that resolves
to this value.

Interceptors are maps with any of (all are optional):

- `::action/enter` - called with the context map when entering the interceptor.
  Returns a new (or the same) context map, or a chainable that resolves to
  a context map.
- `::action/leave` - called with the context map when entering the interceptor.
  Returns a new (or the same) context map, or a chainable that resolves to
  a context map.
- `::action/name` - the interceptor name, used to identify it in errors and such.

The `context-stub`, which is the default dispatch context, has the following:

- `::action/action` - the action being dispatched.
- `::action/pending-effects` - a FIFO queue of pending effects.
- `::action/pending-enter` - a FIFO queue of interceptors to enter.
- `::action/pending-leave` - a vector of interceptors that have
  been entered, and need to be left.

These entries _must_ be available in the context for dispatch to
continue; however they can be manipulated by middleware.

When an error is thrown (in middleware, injector, executor, etc)
the error is added to `::action/errors` (in the context map), and
`::action/pending-enter` is cleared. The errors map is a nested
map of the form `interceptor-name -> interceptor-stage -> error`.
Where `interceptor-stage` is the stage the interceptor was in
when the error was thrown (either `::action/enter` or `::action/leave`).

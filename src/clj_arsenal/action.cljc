(ns clj-arsenal.action
  #?(:cljs (:require-macros clj-arsenal.action))
  #?(:cljd/clj-host (:host-ns (:require [clj-arsenal.basis :as b])))
  (:refer-clojure :exclude [quote unquote])
  (:require
   #?(:cljd [cljd.core :refer [IFn]])
   [clojure.walk :as walk]
   [clj-arsenal.basis.queue :refer [empty-queue]]
   [clj-arsenal.basis :refer [err? schedule-once chain chainable m] :as b]
   [clj-arsenal.check :refer [check expect]]
   [clj-arsenal.log :refer [log spy]]))

(defrecord Action [headers effects])
(defrecord Injection [kind args])
(defrecord ^:private Literal [v])

(defn- continue-dispatch
  [{pending-enter ::pending-enter pending-leave ::pending-leave :as context} on-finish]
  (cond
    (seq pending-enter)
    (let [interceptor (peek pending-enter)
          interceptor (cond-> interceptor (nil? (::name interceptor)) (assoc ::name (keyword (gensym "anon"))))
          enter-fn (::enter interceptor)
          next-context (assoc context ::pending-enter (pop pending-enter) ::pending-leave (conj pending-leave interceptor))]
      (if-not (ifn? enter-fn)
        (continue-dispatch next-context on-finish)
        (chain
          (m
            (enter-fn next-context)
            :catch b/err-any err
            err)
          (fn [new-context]
            (continue-dispatch
              (if (err? new-context)
                (-> next-context
                  (assoc-in [::errors (::name interceptor) ::enter] new-context)
                  (assoc ::pending-enter empty-queue))
                new-context)
              on-finish)))))
    
    (seq pending-leave)
    (let [interceptor (peek pending-leave)
          leave-fn (::leave interceptor)
          next-context (assoc context ::pending-leave (pop pending-leave))]
      (if-not (ifn? leave-fn)
        (continue-dispatch next-context on-finish)
        (chain
          (m 
            (leave-fn next-context)
            :catch b/err-any err
            err)
         (fn [new-context]
           (continue-dispatch
             (if (err? new-context)
               (assoc-in next-context [::errors (::name interceptor) ::leave] new-context)
               new-context)
             on-finish)))))
    
    :else
    (on-finish context))
  nil)

(declare ->Dispatcher dispatcher-call)

(deftype Dispatcher [dispatch-fn base-context]
  #?@(:cljs
      [IFn
       (-invoke [this]
         (dispatcher-call this []))
       (-invoke [this x]
        (dispatcher-call this [x]))
       (-invoke [this x1 x2]
        (dispatcher-call this [x1 x2]))
       (-invoke [this x1 x2 x3]
        (dispatcher-call this [x1 x2 x3]))
       (-invoke [this x1 x2 x3 x4]
        (dispatcher-call this [x1 x2 x3 x4]))
       (-invoke [this x1 x2 x3 x4 x5]
        (dispatcher-call this [x1 x2 x3 x4 x5]))
       (-invoke [this x1 x2 x3 x4 x5 x6]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6]))
       (-invoke [this x1 x2 x3 x4 x5 x6 x7]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7]))
       (-invoke [this x1 x2 x3 x4 x5 x6 x7 x8]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8]))
       (-invoke [this x1 x2 x3 x4 x5 x6 x7 x8 x9]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9]))
       (-invoke [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10]))
       (-invoke [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11]))
       (-invoke [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12]))
       (-invoke [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13]))
       (-invoke [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14]))
       (-invoke [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15]))
       (-invoke [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16]))
       (-invoke [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17]))
       (-invoke [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18]))
       (-invoke [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19]))
       (-invoke [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20]))
       (-invoke [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 args]
        (dispatcher-call this (into [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20] args)))]

      :cljd
      [IFn
       (-invoke [this]
        (dispatcher-call this []))
       (-invoke [this x]
        (dispatcher-call this [x]))
       (-invoke [this x1 x2]
        (dispatcher-call this [x1 x2]))
       (-invoke [this x1 x2 x3]
        (dispatcher-call this [x1 x2 x3]))
       (-invoke [this x1 x2 x3 x4]
        (dispatcher-call this [x1 x2 x3 x4]))
       (-invoke [this x1 x2 x3 x4 x5]
        (dispatcher-call this [x1 x2 x3 x4 x5]))
       (-invoke [this x1 x2 x3 x4 x5 x6]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6]))
       (-invoke [this x1 x2 x3 x4 x5 x6 x7]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7]))
       (-invoke [this x1 x2 x3 x4 x5 x6 x7 x8]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8]))
       (-invoke [this x1 x2 x3 x4 x5 x6 x7 x8 x9]
        (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9]))
       (-invoke-more [this x1 x2 x3 x4 x5 x6 x7  x8 x9 rest]
         (dispatcher-call this (into [x1 x2 x3 x4 x5 x6 x7 x8 x9] rest)))
       (-apply [this more]
         (dispatcher-call this more))]

      :clj
      [clojure.lang.IFn
       (invoke
         [this]
         (dispatcher-call this []))
       (invoke
         [this x]
         (dispatcher-call this [x]))
       (invoke
         [this x1 x2]
         (dispatcher-call this [x1 x2]))
       (invoke
         [this x1 x2 x3]
         (dispatcher-call this [x1 x2 x3]))
       (invoke
         [this x1 x2 x3 x4]
         (dispatcher-call this [x1 x2 x3 x4]))
       (invoke
         [this x1 x2 x3 x4 x5]
         (dispatcher-call this [x1 x2 x3 x4 x5]))
       (invoke
         [this x1 x2 x3 x4 x5 x6]
         (dispatcher-call this [x1 x2 x3 x4 x5 x6]))
       (invoke
         [this x1 x2 x3 x4 x5 x6 x7]
         (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7]))
       (invoke
         [this x1 x2 x3 x4 x5 x6 x7 x8]
         (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8]))
       (invoke
         [this x1 x2 x3 x4 x5 x6 x7 x8 x9]
         (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9]))
       (invoke
         [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10]
         (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10]))
       (invoke
         [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11]
         (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11]))
       (invoke
         [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12]
         (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12]))
       (invoke
         [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13]
         (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13]))
       (invoke
         [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14]
         (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14]))
       (invoke
         [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15]
         (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15]))
       (invoke
         [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16]
         (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16]))
       (invoke
         [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17]
         (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17]))
       (invoke
         [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18]
         (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18]))
       (invoke
         [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19]
         (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19]))
       (invoke
         [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20]
         (dispatcher-call this [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20]))
       (invoke
         [this x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 args]
         (dispatcher-call this (into [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20] args)))
       (applyTo
         [this args]
         (dispatcher-call this args))]))

(defn- dispatcher-call
  [^Dispatcher dispatcher args]
  ((.-dispatch-fn dispatcher) dispatcher (.-base-context dispatcher) (first args) (rest args)))

(defn dispatcher "
Create a dispatcher.  `interceptors` is an ordered collection
of interceptors.  `context-builder` is an optional function
that takes a minimal initial dispatch context, and returns
a modified version of the same.
" [interceptors & {:keys [context-builder dispatch-wrapper] :or {dispatch-wrapper identity}}]
  (let
    [dispatch-impl
     (fn dispatch [dispatcher base-context action args]
       (when-not (instance? Action action)
         (throw (b/err :p ::dispatched-something-not-action :dispatched action)))
       (let
         [context-stub
          {::action action
           ::pending-effects (into empty-queue (.-effects ^Action action))
           ::executed-effects []
           ::pending-enter (into empty-queue interceptors)
           ::pending-leave []
           ::dispatcher dispatcher}

          context-basis
          (merge base-context context-stub)

          context
          (cond
            (ifn? context-builder)
            (apply context-builder context-basis args)

            (seq args)
            (merge context-basis (first args))
            
            :else
            context-basis)]
         (chainable
           (fn [continue]
             (continue-dispatch context continue)))))
     
     dispatch-fn
     (dispatch-wrapper dispatch-impl)]
    (->Dispatcher dispatch-fn {})))

(defn with-extra-context "
Returns a new dispatcher with the given context merged into the current base context.
" [^Dispatcher dispatcher extra-context]
  (->Dispatcher (.-dispatch-fn dispatcher) (merge (.-base-context dispatcher) extra-context)))

(defn with-replace-context "
Returns a new dispatcher with the given context replacing the current base context.
" [^Dispatcher dispatcher new-context]
  (->Dispatcher (.-dispatch-fn dispatcher) new-context))

(defn- apply-injections-substitute
  [injected x]
  (cond
    (instance? Injection x)
    (let [x-injected (get injected x)]
      (if (instance? Literal x-injected)
        (.-v ^Literal x-injected)
        x-injected))
    
    (instance? Literal x)
    (.-v ^Literal x)

    (coll? x)
    (walk/walk
      (partial apply-injections-substitute injected)
      identity
      x)

    :else
    x))

(defn apply-injections
  [injector context form continue]
  (m
    (chain
      (b/chain-all-seq
        (map
          (fn resolve-inj [inj]
            (chainable
              (fn [continue-inner]
                (apply-injections injector context (:args inj)
                  (fn [resolved-args]
                    (if (b/err? resolved-args)
                      (continue-inner [inj resolved-args])
                      (chain
                        (injector context (assoc inj :args resolved-args))
                        (fn [injected]
                          (if-not (instance? Injection injected)
                            (continue-inner [inj injected])
                            (chain
                              (resolve-inj injected)
                              (fn [[_ injected-final]]
                                (continue-inner [inj injected-final]))))))))))))
          (distinct
            (b/gather form #(instance? Injection %)
              :select
              (fn [x]
                (when-not (instance? Literal x)
                  x))))))
      (fn [injected]
        (let
          [injected (into {} injected)
           error (some #(when (b/err? %) %) (vals injected))]
          (if (some? error)
            (continue error)
            (continue (apply-injections-substitute injected form))))))
    :catch b/err-any err
    (continue err)))

(defn- with-effect-error
  [context error]
  (-> context
    (assoc-in [::errors ::effects ::enter] error)
    (assoc ::pending-enter empty-queue)))

(defn flatten-effects
  [effects]
  (mapcat
    (fn [effect]
      (cond
        (or (instance? Injection effect) (and (vector? effect) (seq effect)))
        (list effect)

        (seq? effect)
        (flatten-effects effect)

        (nil? effect)
        nil

        :else
        (throw (b/err :p ::bad-effect ::effect effect))))
    effects))

(defn execute-pending-effects
  [executor injector {pending-effects ::pending-effects executed-effects ::executed-effects :as context} continue]
  (if (empty? pending-effects)
    (continue context)
    (let [next-effect (peek pending-effects)
          next-pending-effects (pop pending-effects)
          next-context (assoc context ::pending-effects next-pending-effects)]
      (m
        (apply-injections injector next-context next-effect
          (fn handle-resolved-effects [resolved-next-effects]
            (cond
              (or (nil? resolved-next-effects)
                (and (seq? resolved-next-effects)
                  (empty? resolved-next-effects)))
              (execute-pending-effects executor injector next-context continue)

              (instance? Injection resolved-next-effects)
              (apply-injections injector next-context resolved-next-effects handle-resolved-effects)

              (seq? resolved-next-effects)
              (execute-pending-effects
                executor injector
                (assoc next-context ::pending-effects
                  (into empty-queue (concat (flatten-effects resolved-next-effects) next-pending-effects)))
                continue)

              (err? resolved-next-effects)
              (do
                (continue (with-effect-error next-context resolved-next-effects))
                nil)

              (and (vector? resolved-next-effects) (seq resolved-next-effects))
              (if (instance? Injection next-effect)
                (execute-pending-effects
                  executor injector
                  (assoc next-context ::pending-effects
                    (into empty-queue (cons resolved-next-effects next-pending-effects)))
                  continue)
                (let [effect resolved-next-effects
                      next-context (assoc next-context ::executed-effects (conj executed-effects effect))]
                  (chain
                    (m
                      (executor next-context effect)
                      :catch b/err-any err
                      err)
                    (fn [new-context]
                      (if (err? new-context)
                        (continue (with-effect-error next-context new-context))
                        (execute-pending-effects executor injector new-context continue))))))

              :else
              (throw (b/err :p ::bad-effect :effect resolved-next-effects)))))
        :catch b/err-any err
        (with-effect-error next-context err)))))

(defn effects "
Creates an interceptor for executing effects.
" [executor injector]
  {::name ::effects

   ::enter
   (fn [context]
     (chainable
       (fn [continue]
         (execute-pending-effects executor injector context continue))))})

(defn errors "
Creates an interceptor to log unhandled errors with clj-arsenal.log.
" []
  {::name ::errors

   ::leave
   (fn [context]
     (doseq
       [[interceptor-name error-map] (::errors context)
        [error-stage error-val] error-map

        :when
        (err? error-val)

        :let
        [data (b/err-data error-val)
         action (::action context)
         action-key (-> action :headers :key)]

        :when
        (or (nil? data) (not (:no-log data)))]
       (log :error
         :interceptor interceptor-name
         :stage error-stage
         :action (or action-key action)
         :ex error-val
         :st (:st data)))
     context)})

(defn act "
Create an action.  Use like `(act {:as headers} & effects)` or `(act & effects)`,
where each effect is a vector with an effect kind, and zero or more args.
" [& items]
  (let [[headers effects]
        (if (and (map? (first items)) (not (instance? Injection (first items))))
          [(first items) (rest items)]
          [{} items])

        effects
        (mapcat
          (fn flatten-effects [effect]
            (cond
              (or (instance? Injection effect) (and (vector? effect) (seq effect)))
              [effect]

              (seq? effect)
              effect

              (nil? effect)
              nil

              :else
              (throw (b/err :p ::bad-effect ::effect effect))))
          effects)]
    (->Action headers effects)))

(defn action? "
Returns true if `x` is an action.
" [x]
  (instance? Action x))

(defn << "
Creates an injection with a depth of 0.
" [kind & args]
  (->Injection kind (vec args)))

(defn decide "
Creates an ::decide injector.

```
(decide
  (fn [x y]
    (if (= x y)
      [:my-effecy x]
      [:my-other-effect x y]))
  (<< :inject-x)
  (<< :inject-y))
```

Decision injections evaluate the given pure
function, with the provided (generally injected)
arguments.  Injecting the result.
" [& args]
  (->Injection ::decide (vec args)))

(defn <<ctx "
Creates a ::context injector.

```
(<<ctx :path :to :context :value)
```

These simply inject a value from the current context.
" [& path]
  (->Injection ::context (vec path)))

(defn <<err "
Creates an ::error injector, which injects an
error with in a way that doesn't cause failure.
" [& data]
  (->Injection ::error (vec data)))

(defn quote "
Quotes `form`, preventing injections from being applied.
" [form]
  (->Literal form))

(defn unquote "
Unquote `form`.
" [form]
  (if (instance? Literal form)
    (.-v ^Literal form)
    form))

(defn injection? "
Returns true if `x` is an injection.
" [x]
  (instance? Injection x))

(defn with-default-injector "
Wraps your custom injector in one that handles built-in
injections like those produced by `<<ctx`, `with-inj`,
`decide`.
" [injector]
  (fn [context {:keys [kind args] :as injection}]
    (case kind
      ::decide (apply (first args) (rest args))
      ::context (->Literal (get-in context (vec args)))
      ::error (->Literal (apply b/err args))
      (injector context injection))))

(defn- ^:macro-support gather-names
  [form]
  (b/gather form symbol?))

(defn- ^:macro-support with-inj*
  [binding-pairs body]
  (loop
    [bound-names #{}
     accepted-binding-pairs []
     remaining-binding-pairs binding-pairs]
    (cond
      (empty? remaining-binding-pairs)
      (cond
        (empty? accepted-binding-pairs)
        body

        :else
        `(decide
           (fn [~@(map first accepted-binding-pairs)]
             ~@body)
           ~@(map second accepted-binding-pairs)))
      
      :else
      (let
        [[bind-pattern bind-expr :as next-binding-pair] (first remaining-binding-pairs)
         expr-names (gather-names bind-expr)]
        (cond
          (some bound-names expr-names)
          `(decide
             (fn [~@(map first accepted-binding-pairs)]
               ~(with-inj* remaining-binding-pairs body))
             ~@(map second accepted-binding-pairs))
          
          :else
          (recur
            (into bound-names (gather-names bind-pattern))
            (conj accepted-binding-pairs next-binding-pair)
            (rest remaining-binding-pairs)))))))

#?(:clj
   (defmacro with-inj "
Syntax sugar around `decide`.

```
(with-inj [foo (<<ctx :foo)
           bar (<<ctx :bar)]
 (+ foo bar))
```
" {:clj-kondo/lint-as 'clojure.core/let}
  [bindings & body]
  {:pre [(vector? bindings) (even? (count bindings))]}
  (with-inj* (partition 2 bindings) body)))

(check ::simple-dispatch
  (let [executor (fn [context [kind & args]]
                   (case kind
                     :foo (assoc context :result (into [:foo] args))
                     :bar (assoc context :result (into [:bar] args))))
        injector (fn [context {:keys [kind args]}]
                   (case kind
                     :exact (first args)))
        dispatch (dispatcher [(effects executor injector)])]
    (chainable
      (fn [continue]
        (chain (dispatch (act [:foo (<< :exact "blah")]))
          (fn [new-context]
            (expect = (:result new-context) [:foo "blah"])
            (chain (dispatch (act [:bar (<< :exact "blah")]))
              (fn [new-context]
                (expect = (:result new-context) [:bar "blah"])
                (continue nil)))))))))

(check ::enter-leave-with-chained-middleware
  (let [dispatch
        (dispatcher
          [{::enter
            (fn [context]
              (chainable
                (fn [continue]
                  (continue (assoc context ::entered-1 true)))))
            ::leave
            (fn [context]
              (chainable
                (fn [continue]
                  (continue (assoc context ::left-1 true)))))}
           {::enter
            (fn [context]
              (chainable
                (fn [continue]
                  (continue (assoc context ::entered-2 true)))))
            ::leave
            (fn [context]
              (chainable
                (fn [continue]
                  (continue (assoc context ::left-2 true)))))}])]
    (chainable
      (fn [continue]
        (chain (dispatch (act [:foo]))
          (fn [new-context]
            (expect true? (::entered-1 new-context))
            (expect true? (::entered-2 new-context))
            (expect true? (::left-1 new-context))
            (expect true? (::left-2 new-context))
            (continue nil)))))))

(check ::deep-injections
  (let [injector
        (fn [_context {:keys [kind args]}]
          (case kind
            :exact (first args)))]
    (chainable
      (fn [continue]
        (apply-injections injector nil [(<< :exact "foo") (clj-arsenal.action/quote (<< :exact "bar"))]
          (fn [resolved]
            (m
              (expect = resolved ["foo" (<< :exact "bar")])
              (continue nil)
              :catch b/err-any err
              (continue err))))))))

(check ::decide-effects
  (let
    [dispatch
     (dispatcher
       [(effects
          (fn executor
            [context & _args]
            context)
          (with-default-injector
            (fn injector
              [_context {:keys [kind]}]
              (chainable
                (fn [continue]
                  (schedule-once 1 continue kind))))))])]
    (chainable
      (fn [continue]
        (chain
          (dispatch
            (act
              (decide
                (fn [x y]
                  (list
                    [:x x]
                    [:y y]))
                (<< :x)
                (<< :y))))
          (fn [{executed-effects ::executed-effects :as context}]
            (m
              (expect = executed-effects [[:x :x] [:y :y]])
              (continue nil)
              :catch b/err-any err
              (continue err))))))))

(check ::flatten-injected-effects
  (let [dispatch
        (dispatcher
          [(effects
             (fn executor
               [context & _args]
               context)
             (with-default-injector
               (fn injector
                 [_context {:keys [kind]}]
                 kind)))])]
    (chainable
      (fn [continue]
        (chain
          (dispatch
            (act
              (decide
                (fn [x y z]
                  (list
                    [:x x]
                    [:y y]
                    nil
                    (list
                      [:z z])))
                (<< :x)
                (<< :y)
                (<< :z))))
          (fn [{executed-effects ::executed-effects}]
            (m
              (expect = executed-effects [[:x :x] [:y :y] [:z :z]])
              (continue nil)
              :catch b/err-any err
              (continue err))))))))

(check ::recursive-injection
  (let [dispatch
        (dispatcher
          [(effects
             (fn executor
               [context & _args]
               context)
             (with-default-injector
               (fn injector
                 [_context {:keys [kind]}]
                 [kind kind])))])]
    (chainable
      (fn [continue]
        (chain
          (dispatch
            (act
              (decide
                (fn []
                  (<< :x)))))
          (fn [{executed-effects ::executed-effects}]
            (m
              (expect = executed-effects [[:x :x]])
              (continue nil)
              :catch b/err-any err
              (continue err))))))))

(check ::with-inj
  (let
    [dispatch
     (dispatcher
       [(effects
          (fn executor
            [context & _args]
            context)
          (with-default-injector
            (fn injector
              [_context {:keys [kind]}]
              kind)))])]
    (chainable
      (fn [continue]
        (chain
          (dispatch
            (act
              (clj-arsenal.action/with-inj
                [x (<< :x)
                 y (<< :y)]
                (list
                  [x]
                  [y]))))
          (fn [{executed-effects ::executed-effects}]
            (m
              (expect = executed-effects [[:x] [:y]])
              (continue nil)
              :catch b/err-any err
              (continue err))))))))

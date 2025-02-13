(ns clj-arsenal.action
  #?(:cljs (:require-macros clj-arsenal.action))
  (:require
   [clojure.walk :as walk]
   [clj-arsenal.basis.queue :refer [empty-queue]]
   [clj-arsenal.basis.protocols.chain :refer [chain chain-all chainable]]
   [clj-arsenal.basis :refer [try-fn error? schedule-once] :as basis]
   [clj-arsenal.check :refer [check expect]]
   [clj-arsenal.log :refer [log spy]]))

(defrecord Action [headers effects])
(defrecord Injection [depth kind args])

(defn- inject 
  [{:keys [kind args] :as injection} injector context]
  (case kind
    ::decide (apply (first args) (rest args))
    ::context (get-in context (vec args))
    (injector context injection)))

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
        (chain (try-fn #(enter-fn next-context) :catch identity)
          (fn [new-context]
            (continue-dispatch
              (if (error? new-context)
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
        (chain (try-fn #(leave-fn next-context) :catch identity)
         (fn [new-context]
           (continue-dispatch
             (if (error? new-context)
               (assoc-in next-context [::errors (::name interceptor) ::leave] new-context)
               new-context)
             on-finish)))))
    
    :else
    (on-finish context))
  nil)

(defn dispatcher "
Create a dispatcher.  `interceptors` is an ordered collection
of interceptors.  `context-builder` is an optional function
that takes a minimal initial dispatch context, and returns
a modified version of the same.
" [interceptors & {:keys [context-builder]}]
  (fn dispatch [action & context-builder-params]
    (when-not (instance? Action action)
      (throw (ex-info "dispatched something other than an action" {:dispatched action})))
    (let [context-stub {::action action
                        ::pending-effects (into empty-queue (.-effects ^Action action))
                        ::executed-effects []
                        ::pending-enter (into empty-queue interceptors)
                        ::pending-leave []}
          context (if (ifn? context-builder)
                    (apply context-builder context-stub context-builder-params)
                    context-stub)]
      (chainable
        (fn [continue]
          (continue-dispatch context continue))))))

(defn- apply-injections
  [injector context form continue]
  (chain-all form continue
    :mapper
    (fn [x]
      (if-not (instance? Injection x)
        x
        (if (pos? (:depth x))
          (update x :depth dec)
          (inject x injector context))))))

(defn- with-effect-error
  [context error]
  (-> context
    (assoc-in [::errors ::effects ::enter] error)
    (assoc ::pending-enter empty-queue)))

(defn execute-pending-effects
  [executor injector {pending-effects ::pending-effects executed-effects ::executed-effects :as context} continue]
  (if (empty? pending-effects)
    (continue context)
    (let [next-effect (peek pending-effects)
          next-pending-effects (pop pending-effects)
          next-context (assoc context ::pending-effects next-pending-effects)]
      (try-fn
        (fn []
          (apply-injections injector next-context next-effect
            (fn [resolved-next-effects]
              (cond
                (or (nil? resolved-next-effects)
                  (and (seq? resolved-next-effects)
                    (empty? resolved-next-effects)))
                (execute-pending-effects executor injector next-context continue)

                (seq? resolved-next-effects)
                (execute-pending-effects
                  executor injector
                  (assoc next-context ::pending-effects
                    (into empty-queue (concat resolved-next-effects next-pending-effects)))
                  continue)

                (error? resolved-next-effects)
                (do
                  (continue resolved-next-effects)
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
                      (try-fn
                        #(executor next-context effect)
                        :catch identity)
                      (fn [new-context]
                        (if (error? new-context)
                          (continue (with-effect-error next-context new-context))
                          (execute-pending-effects executor injector new-context continue))))))

                :else
                (throw (ex-info "invalid effect" {:p ::bad-effect :effect resolved-next-effects}))))))
        :catch #(with-effect-error next-context %)))))

(defn effects "
Creates an interceptor for executing effects.
" [executor injector]
  {::name ::effects
   ::enter (fn [context]
             (chainable
               (fn [continue]
                 (execute-pending-effects executor injector context continue))))})

(defn errors "
Creates an interceptor to log unhandled errors with clj-arsenal.log.
" []
  {::name ::errors
   ::leave (fn [context]
             (doseq [[interceptor-name error-map] (::errors context)
                     [error-stage error-val] error-map
                     :when (error? error-val)
                     :let [data (ex-data error-val)]
                     :when (or (nil? data) (not (:no-doc (meta data))))]
               (log :error
                 :msg (case error-stage
                        ::enter "error entering interceptor"
                        ::leave "error leaving interceptor"
                        "error in interceptor")
                 :interceptor interceptor-name
                 :action-key (-> context ::action :headers :key)
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
        effects (mapcat
                  (fn flatten-effects [effect]
                    (cond
                      (or (instance? Injection effect) (and (vector? effect) (seq effect)))
                      [effect]
                      
                      (seq? effect)
                      effect
                      
                      (nil? effect)
                      nil
                      
                      :else
                      (throw (ex-info "invalid effect" {:problem ::bad-effect ::effect effect}))))
                  effects)]
    (->Action headers effects)))

(defn action? "
Returns true if `x` is an action.
" [x]
  (instance? Action x))

(defn << "
Creates an injection with a depth of 0.
" [kind & args]
  (->Injection 0 kind (vec args)))

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
  (->Injection 0 ::decide (vec args)))

(defn <<ctx "
Creates a ::context injector.

```
(<<ctx :path :to :context :value)
```

These simply inject a value from the current context.
" [& path]
  (->Injection 0 ::context (vec path)))

(defn inc-depth "
Walks form, incrementing the depth of all injections.
" [form]
  (walk/postwalk
    (fn [x]
      (if (instance? Injection x)
        (update x :depth inc)
        x))
    form))

(defn dec-depth "
Walks form, decrementing the depth of all injections.
" [form]
  (walk/postwalk
    (fn [x]
      (if (instance? Injection x)
        (update x :depth dec)
        x))
    form))

(defn injection? "
Returns true if `x` is an injection.
" [x]
  (instance? Injection x))

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
  (let [binding-pairs (partition 2 bindings)]
    `(decide
       (fn [~@(map first binding-pairs)] ~@body)
       ~@(map second binding-pairs)))))

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
        (apply-injections injector nil [(<< :exact "foo") (inc-depth (<< :exact "bar"))]
          (fn [resolved]
            (try-fn
              (fn []
                (expect = resolved ["foo" (<< :exact "bar")])
                (continue nil))
              :catch continue)))))))

(check ::decide-effects
  (let [dispatch
        (dispatcher
          [(effects
             (fn executor
               [context & _args]
               context)
             (fn injector
               [_context & {:keys [kind]}]
               (chainable
                 (fn [continue]
                   (schedule-once 1 continue kind)))))])]
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
            (try-fn
              (fn []
                (expect = executed-effects [[:x :x] [:y :y]])
                (continue nil))
              :catch continue)))))))

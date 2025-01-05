(ns clj-arsenal.action
  (:require
   [clojure.walk :as walk]
   [clj-arsenal.basis.queue :refer [empty-queue]]
   [clj-arsenal.basis.protocols.chain :refer [chain chain-all chainable]]
   [clj-arsenal.basis :refer [try-fn error?] :as basis]
   [clj-arsenal.check :refer [check expect]]
   [clj-arsenal.log :refer [log spy]]))

(defrecord Action [headers effects])
(defrecord Injection [depth kind args])

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
                        ::pending-enter (into empty-queue interceptors)
                        ::pending-leave []}
          context (if (ifn? context-builder)
                    (apply context-builder context-stub context-builder-params)
                    context-stub)]
      (chainable
        (fn [continue]
          (continue-dispatch context continue))))))

(defn- apply-injections
  [injector context form]
  (chain-all form
    :mapper (fn [x]
              (if-not (instance? Injection x)
                x
                (if (pos? (:depth x))
                  (update x :depth dec)
                  (injector context x))))))

(defn- execute-pending-effects
  [executor injector {pending-effects ::pending-effects :as context}]
  (if (empty? pending-effects)
    context
    (chainable
      (fn [continue]
        (let [next-effect (peek pending-effects)
              next-context (assoc context ::pending-effects (pop pending-effects))]
          (try-fn
            (fn []
              (chain (apply-injections injector next-context next-effect)
                (fn [resolved-next-effect]
                  (if (error? resolved-next-effect)
                    (continue resolved-next-effect)
                    (chain (try-fn #(executor next-context resolved-next-effect) :catch identity)
                      (fn [new-context]
                        (if (error? new-context)
                          (continue new-context)
                          (chain (execute-pending-effects executor injector new-context) continue))))))))
            :catch continue))))))

(defn effects "
Creates an interceptor for executing effects.
" [executor injector]
  {::name ::effects
   ::enter (partial execute-pending-effects executor injector)})

(defn errors "
Creates an interceptor to log unhandled errors with clj-arsenal.log.
" []
  {::name ::errors
   ::leave (fn [context]
             (doseq [[interceptor-name {enter-error ::enter leave-error ::leave}] (::errors context)]
               (when (error? enter-error)
                 (log :error :msg "error entering interceptor" :interceptor interceptor-name :ex enter-error))
               (when (error? leave-error)
                 (log :error :msg "error leaving interceptor" :interceptor interceptor-name :ex leave-error)))
             context)})

(defn act "
Create an action.  Use like `(act {:as headers} & effects)` or `(act & effects)`,
where each effect is a vector with an effect kind, and zero or more args.
" [& items]
  (let [[headers effects] (if (map? (first items)) [(first items) (rest items)] [{} items])
        effects (mapcat
                  (fn flatten-effects [effect]
                    (cond
                      (and (vector? effect) (seq effect))
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

(defn <<< "
Creates an injection with an arbitrary nesting depth.
The depth of an injection determines when it's evaluated.
If the depth is <= 0 when an effect is executed then the
injection is resolved via the injector; otherwise it resolves
to the same injection with its depth decremented.
" [depth kind & args]
  (->Injection depth kind (vec args)))

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
  (let [dispatch (dispatcher
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
  (let [injector (fn [context {:keys [kind args]}]
                   (case kind
                     :exact (first args)))]
    (chainable
      (fn [continue]
        (apply-injections injector nil [(<< :exact "foo") (<<< :exact "bar")]
          (fn [resolved]
            (try-fn
              (fn []
                (expect = resolved ["foo" (<< :exact "bar")])
                (continue nil))
              :catch continue)))))))

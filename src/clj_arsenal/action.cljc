(ns clj-arsenal.action
  (:require
   [clojure.walk :as walk]
   [clj-arsenal.basis.queue :refer [empty-queue]]
   [clj-arsenal.basis.protocols.chain :refer [chain chain-all chainable]]
   [clj-arsenal.basis :refer [try-fn error? schedule-once]]
   [clj-arsenal.check :refer [check expect]]))

(defrecord Action [headers effects])
(defrecord Injection [kind args])

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

(defn dispatcher
  [interceptors & {:keys [context-builder]}]
  (fn dispatch [^Action action & context-builder-params]
    (let [context-stub {::action action
                        ::pending-effects (into empty-queue (.-effects action))
                        ::pending-enter (into empty-queue interceptors)
                        ::pending-leave []}
          context (if (ifn? context-builder)
                    (apply context-builder context-stub context-builder-params)
                    context-stub)]
      (chainable
        (fn [continue]
          (schedule-once 0 #(continue-dispatch context continue)))))))

(defn apply-injections
  [injector context form continue]
  (chain-all form continue
    :walker walk/postwalk
    :mapper (fn [x]
              (if-not (instance? Injection x)
                x
                (let [defer-depth (or (some-> x meta ::depth) 0)]
                  (if (pos? defer-depth)
                    (vary-meta x update ::depth dec)
                    (injector context x)))))))

(defn execute-pending-effects
  [executor injector {pending-effects ::pending-effects :as context}]
  (if (empty? pending-effects)
    context
    (chainable
      (fn [continue]
        (let [next-effect (peek pending-effects)
              next-context (assoc context ::pending-effects (pop pending-effects))]
          (try-fn
            (fn []
              (apply-injections injector next-context next-effect
                (fn [resolved-next-effect]
                  (chain (try-fn #(executor next-context resolved-next-effect) :catch identity)
                    (fn [new-context]
                      (if (error? new-context)
                        new-context
                        (chain (execute-pending-effects executor injector new-context) continue)))))))
            :catch continue))))))

(defn effects "
Creates an interceptor for executing effects.
" [executor injector]
  {::name ::effects
   ::enter (partial execute-pending-effects executor injector)})

(defn act
  [& items]
  (let [[headers effects] (if (map? (first items)) [(first items) (rest items)] [{} items])]
    (when-some [bad-effect (some #(not (or (vector? %) (seq %))) (remove nil? effects))]
      (throw
        (ex-info "bad effect, must be a non-empty vector"
          {::kind ::bad-effect
           ::effect bad-effect})))
    (->Action headers effects)))

(defn action?
  [x]
  (instance? Action x))

(defn <<
  [kind & args]
  (->Injection kind (vec args)))

(defn <<<
  [kind & args]
  (with-meta (->Injection kind (vec args)) {::depth 1}))

(defn injection?
  [x]
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

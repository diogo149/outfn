(ns outfn.generate
  (:require [clojure.set :as set]
            [outfn.util :as util]
            [outfn.implicits :as implicits]
            [outfn.state :as state]
            [slingshot.slingshot :as ss]))

;; -------------------------
;; creation of the functions
;; -------------------------

(defn make-generated-fn-name
  "Returns a name for the generated function, for ease of debugging"
  [outfn-name args]
  {:pre [(vector? args)
         (every? symbol? args)]}
  (->> args
       seq
       (cons "outfn")
       (cons outfn-name)
       (interpose "-")
       clojure.string/join
       symbol))

(defn make-generated-fn
  "TODO"
  [outfn-name arg-set prepost-map fn-body]
  {:pre [(set? arg-set)
         (every? symbol? arg-set)]}
  (let [args (->> arg-set
                  sort
                  (map name)
                  (mapv symbol))
        ;; create a name for the anonymous function for easier debugging
        fn-name (make-generated-fn-name outfn-name args)]
    (eval `(fn ~fn-name ~args ~prepost-map ~@fn-body))))

(defn generate-fn!
  "Generate a function for a function map, and save it"
  [fn-map]
  (let [outfn-name (util/safe-get fn-map :outfn-name)
        arg-set (util/safe-get fn-map :arg-set)
        prepost-map (util/safe-get fn-map :prepost-map)
        fn-body (util/safe-get fn-map :fn-body)
        f (make-generated-fn outfn-name arg-set prepost-map fn-body)]
    (assert (fn? f))
    (state/save-fn-map! (util/safe-assoc fn-map :fn f))))

;; ---------------------------
;; calling generated functions
;; ---------------------------

(defn generate-fn-call
  [f args extra-data]
  (let [e (gensym "e")
        new-extra-data (assoc extra-data :thrown e)]
    `(ss/try+
      (~f ~@args)
      (catch Object ~e
        (ss/throw+ ~new-extra-data)))))

(defn positional-fn-call
  "Returns the positional arguments for an outfn, if any for the given arg-map"
  [outfn-var input-kws arg-map]
  {:pre [(var? outfn-var)
         (map? arg-map)
         (set? input-kws)
         (every? keyword? (keys arg-map))]}
  (let [input-sets (state/get-input-sets outfn-var)
        selected-input-set (if (some #{input-kws} input-sets)
                             ;; prefer an exact match if possible
                             input-kws
                             ;; else take the first match
                             (first (filter #(every? input-kws %) input-sets)))]
    (when selected-input-set
      (let [f (state/get-fn outfn-var selected-input-set)
            positional-args (map arg-map (sort selected-input-set))]
        ;; this assertion should never fail, because the input sets are keys
        ;; in a map to their respective functions
        (assert f (format "ERROR: outfn not found for %s with keys %s"
                          outfn-var
                          selected-input-set))
        ;; position arguments appropriately
        (generate-fn-call f positional-args {:error-time :runtime
                                             :outfn-var outfn-var
                                             :input-keys selected-input-set})))))

(defn var->computations
  [v]
  {:pre [(var? v)]
   ;; TODO validate computation
   }
  (let [common-data-map (state/get-common-data v)
        output-kw (util/safe-get common-data-map :output)
        input-sets (state/get-input-sets v)]
    (for [input-set input-sets]
      {:inputs input-set
       :var v
       :output output-kw})))

(defn output-var->computations
  [v]
  {:pre [(var? v)]
   ;; TODO validate computation
   }
  (let [common-data-map (state/get-common-data v)
        output-kw (or (:output common-data-map)
                      ;; this allows the output key to be optional and
                      ;; implicits to still work
                      ::output)
        input-sets (state/get-input-sets v)]
    (for [input-set input-sets]
      {:inputs input-set
       :var v
       :output output-kw})))



(defn build-serial-call
  [outfn-var input-kws output-kw computations order arg-map]
  {:pre [(var? outfn-var)
         (every? keyword? input-kws)
         (set? input-kws)
         (keyword? output-kw)
         ;; TODO validate computations
         ;; TODO validate order
         (map? arg-map)
         (every? keyword? (keys arg-map))]}
  (let [computation-pair->var (into {}
                                    (for [{:keys [var inputs output]}
                                          computations]
                                      [[output inputs] var]))
        computation-pair->fn (fn [pair]
                               (state/get-fn (computation-pair->var pair)
                                             (second pair)))
        ;; NOTE: this will remove unused inputs, making it lazy
        used-input-kws (->> order
                            (mapcat second)
                            (filter input-kws)
                            distinct)
        computed-kws (mapv first order)
        kw->sym (into {} (for [kw (concat used-input-kws computed-kws)]
                           [kw (gensym (name kw))]))

        input-let-pairs
        (doall (for [kw used-input-kws]
                 [(kw->sym kw) (util/safe-get arg-map kw)]))

        computed-let-pairs
        (doall (for [[o-kw i-kws :as pair] order]
                 (let [intermediate-var (computation-pair->var pair)]
                   [;; return the symbol to assign the value to
                    (kw->sym o-kw)
                    ;; return the function call corresponding to
                    (generate-fn-call (computation-pair->fn pair)
                                      (->> i-kws
                                           sort
                                           (map kw->sym))
                                      ;; lots of debugging information that
                                      ;; will hopefully come in handy
                                      {:error-time :runtime
                                       :outfn-var outfn-var
                                       :computation-order order
                                       :intermediate-var intermediate-var
                                       ;; input-keys and computation step
                                       ;; are somewhat redundant
                                       :computation-step pair
                                       :input-keys i-kws})])))]
    `(let ~(vec (apply concat (concat input-let-pairs computed-let-pairs)))
       ~(kw->sym output-kw))))

(defn implicit-fn-call
  "TODO"
  [outfn-var input-kws arg-map]
  {:pre [(every? keyword? input-kws)
         (set? input-kws)
         (var? outfn-var)
         (map? arg-map)]}
  (let [common-data-map (state/get-common-data outfn-var)
        ;; TODO validate common-data-map
        _ (assert (contains? common-data-map :implicits)
                  (format "Invalid set of keys %s for %s"
                          input-kws
                          outfn-var))
        implicits-var-set (util/safe-get common-data-map :implicits)
        output-computations (output-var->computations outfn-var)
        output-kw (->> output-computations first :output)
        _ (assert (keyword? output-kw))
        ;; TODO dedupe computations with the same input and output, select
        ;; the cheapest
        available-computations (concat (mapcat var->computations
                                               implicits-var-set)
                                       output-computations)
        computation-graph (implicits/compute-implicits input-kws
                                                       output-kw
                                                       available-computations)
        order (implicits/generate-serial-order input-kws
                                               output-kw
                                               computation-graph)]
    (build-serial-call outfn-var
                       input-kws
                       output-kw
                       available-computations
                       order
                       arg-map)))

(defn generated-fn-call
  "TODO"
  [outfn-var arg-map]
  {:pre [(var? outfn-var)
         (map? arg-map)
         (every? keyword? (keys arg-map))]}
  (let [input-kws (set (keys arg-map))]
    (or (positional-fn-call outfn-var input-kws arg-map)
        (implicit-fn-call outfn-var input-kws arg-map))))

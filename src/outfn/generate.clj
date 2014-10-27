(ns outfn.generate
  (:require [outfn.util :as util]
            [outfn.state :as state]))

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

(defn generated-fn-call
  "TODO"
  [outfn-var arg-map]
  {:pre [(var? outfn-var)
         (map? arg-map)
         (every? keyword? (keys arg-map))]}
  (let [input-kws (set (keys arg-map))]
    (if-let [f (state/get-fn outfn-var input-kws)]
      ;; position arguments appropriately
      (cons f (map arg-map (sort input-kws)))
      ;; TODO replace with implicit fn call
      (assert false (format "Invalid set of keys %s for %s"
                            input-kws
                            outfn-var)))))

(ns outfn.implicits
  (:require [loom.graph :as loom]
            [clojure.set :as set]))

;; ------------
;; computation representation
;; ------------

(def default-cost 1)

(defn preprocess-computation
  "Make sure a computation has a cost"
  [{:keys [cost] :as computation}]
  (if cost
    computation
    (assoc computation :cost default-cost)))

(defn preprocess-computations
  "Make sure each computation has a cost"
  [computations]
  (mapv preprocess-computation computations))

;; -----------
;; graph utils
;; -----------

(defn unweighted-graph-merge
  ([g] g)
  ([g1 g2]
     (-> g1
         (loom/add-nodes* (loom/nodes g2))
         (loom/add-edges* (loom/edges g2))))
  ([g1 g2 & gs]
     (reduce unweighted-graph-merge (unweighted-graph-merge g1 g2) gs)))

;; ----------------------------
;; dependency graph computation
;; ----------------------------

(defn greedy-compute-implicits
  [input-kws output-kw available-computations]
  (let [initial-graph (apply loom/digraph input-kws)
        initial-values (into {}
                             (for [kw input-kws]
                               [kw {;; cost is the maximum cost to reach the
                                    ;; node
                                    ;; TODO OPTIMIZE make it the exact cost
                                    :cost 0
                                    ;; dependency graph, where nodes in the
                                    ;; graph are reachable computations
                                    :graph initial-graph}]))
        ;; remove computations that return one of the input values
        computations (remove (comp input-kws :output) available-computations)]
    (loop [{:keys [continue?
                   values
                   reachable-nodes] :as outer-acc} {:continue? true
                                                 :values initial-values
                                                 :reachable-nodes input-kws}]
      (if-not continue?
        (if-let [output-value (get values output-kw)]
          (:graph output-value)
          (do (println "VAL" values output-kw)
            {:error :cannot-be-reached
             :reachable-nodes reachable-nodes}))
        (recur (reduce
                (fn [{:keys [values reachable-nodes] :as acc}
                     {:keys [inputs output cost] :as computation}]
                  (if-not (every? reachable-nodes inputs)
                    acc
                    (let [best-cost (-> values output :cost)
                          current-cost (apply + cost (map #(-> values % :cost)
                                                          inputs))]
                      (if-not (or (nil? best-cost)
                                  (> best-cost current-cost))
                        acc
                        (let [merged-graph (->> inputs
                                                (map (comp :graph values))
                                                (apply unweighted-graph-merge))
                              new-edges (for [input inputs]
                                          [input output])
                              output-graph (loom/add-edges* merged-graph
                                                            new-edges)]
                          ;; TODO recompute cost from graph here, since the
                          ;; computed cost is an upper bound
                          (-> acc
                              (assoc :continue? true)
                              (assoc-in [:values output] {:cost current-cost
                                                          :graph output-graph})
                              (update-in [:reachable-nodes] conj output)))))))
                (assoc outer-acc :continue? false)
                computations))))))

;; TODO output a graph of computations that need to be taken
(defn compute-implicits
  [input-kws output-kw available-computations]
  {:pre [(set? input-kws)
         (every? keyword? input-kws)
         (keyword? output-kw)
         ;; TODO validate available-computations
         ]}
  (let [available-computations (preprocess-computations available-computations)]
    (greedy-compute-implicits input-kws
                              output-kw
                              available-computations)))

;; TODO function that converts graph of computations into clojure code

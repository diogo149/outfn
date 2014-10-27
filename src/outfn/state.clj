(ns outfn.state
  (:require [outfn.util :as util]))

;; TODO is this the right way of storing this state? other alternatives:
;; 1. using the metadata of the macro
;; 2. defining top level functions and using naming conventions
(defonce
  ^{:doc "Atom that contains the stored state for outfn's such as the
generated functions to dispatch to and their inputs and outputs"}
  outfn-state
  (atom {}))

(defn clear-var!
  [outfn-var]
  (swap! outfn-state dissoc outfn-var))

(defn save-fn!
  [outfn-var input-kws f]
  {:pre [(var? outfn-var)
         (set? input-kws)
         (every? keyword? input-kws)
         (ifn? f)]}
  ;; not using util/safe-assoc-in because of re-evaluating code
  (swap! outfn-state assoc-in [outfn-var :fns input-kws] f))

(defn save-fn-map!
  [fn-map]
  (let [outfn-var (util/safe-get fn-map :outfn-var)
        input-kws (util/safe-get fn-map :input-kws)
        f (util/safe-get fn-map :fn)]
    (save-fn! outfn-var input-kws f)
    ;; return nil just so that the whole map isn't returned
    nil))

(defn save-common-data!
  [outfn-var common-data-map]
  {:pre [(var? outfn-var)
         ;; TODO validate common-data-map
         ]}
  (swap! outfn-state assoc-in [outfn-var :common] common-data-map))

(defn get-fn
  [outfn-var input-kws]
  {:pre [(var? outfn-var)
         (set? input-kws)
         (every? keyword? input-kws)]}
  (get-in @outfn-state [outfn-var :fns input-kws]))

(defn get-input-sets
  [outfn-var]
  {:pre [(var? outfn-var)]}
  (keys (get-in @outfn-state [outfn-var :fns])))

(defn get-common-data
  [outfn-var]
  {:pre [(var? outfn-var)]}
  (get-in @outfn-state [outfn-var :common]))

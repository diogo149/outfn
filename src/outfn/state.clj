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

(defn save-fn!
  [outfn-var input-kws f]
  {:pre [(var? outfn-var)
         (set? input-kws)
         (every? keyword? input-kws)
         (ifn? f)]}
  ;; not using util/safe-assoc-in because of re-evaluating code
  (swap! outfn-state assoc-in [outfn-var input-kws] f)
  ;; return nil just so that the whole map isn't returned
  nil)

(defn save-fn-map!
  [fn-map]
  (let [outfn-var (util/safe-get fn-map :outfn-var)
        input-kws (util/safe-get fn-map :input-kws)
        f (util/safe-get fn-map :fn)]
    ;; TODO save more data
    (save-fn! outfn-var input-kws f)))

(defn get-fn
  [outfn-var input-kws]
  {:pre [(var? outfn-var)
         (set? input-kws)
         (every? keyword? input-kws)]}
  (get-in @outfn-state [outfn-var input-kws]))

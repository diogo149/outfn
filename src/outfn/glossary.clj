(ns outfn.glossary
  (:require [outfn.util :as util]
            [schema.core :as s]))

(defn input-kw->symbol
  [input-kw]
  (symbol (name input-kw)))

;; -----------------
;; adding validation
;; -----------------

(defn add-validation?
  [fn-map]
  (cond
   (not (contains? fn-map :glossary)) false
   (contains? fn-map :validate?) (:validate? fn-map)
   :else true))

(defn make-validator
  [glossary input-kw]
  {:pre [(keyword? input-kw)]}
  (let [input-sym (input-kw->symbol input-kw)
        glossary-entry (glossary input-kw)
        {:keys [validator schema]} glossary-entry]
    (assert glossary-entry (format "Invalid key %s: not in provided glossary"
                                   input-kw))
    (remove nil? [(when validator
                    (list validator input-sym))
                  (when schema
                    (list s/validate schema input-sym))])))

(defn add-validators
  [fn-map validators]
  (util/safe-update-in fn-map [:prepost-map :pre] into validators))

(defn update-with-validation
  [fn-map]
  (if-not (add-validation? fn-map)
    fn-map
    (let [glossary (util/safe-get fn-map :glossary)
          _ (assert (ifn? glossary) "Glossary must be a function")
          input-kws (util/safe-get fn-map :input-kws)
          validators (doall (mapcat make-validator (repeat glossary) input-kws))]
      (add-validators fn-map validators))))

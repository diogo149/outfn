(ns outfn.glossary
  (:require [outfn.util :as util]))

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
  (let [glossary-entry (glossary input-kw)
        {:keys [validator schema]} glossary-entry]
    (assert glossary-entry (format "Invalid key %s: not in provided glossary"
                                   input-kw))
    [(when validator
       (list validator input-kw))
     (when schema
       ;; TODO schema validate
       )]))

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
          validators (mapv make-validator (repeat glossary) input-kws)]
      (add-validators fn-map validators))))

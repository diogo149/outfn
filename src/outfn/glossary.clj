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
  [validation-kw glossary input?]
  {:pre [(keyword? validation-kw)]}
  (let [validation-sym (if input?
                         (input-kw->symbol validation-kw)
                         '%)
        glossary-entry (glossary validation-kw)
        {:keys [validator schema]} glossary-entry]
    (assert glossary-entry (format "Invalid key %s: not in provided glossary"
                                   validation-kw))
    (remove nil? [(when validator
                    (list validator validation-sym))
                  (when schema
                    (list s/validate schema validation-sym))])))

(defn add-pre-validators
  [fn-map validators]
  (util/safe-update-in fn-map [:prepost-map :pre] into validators))

(defn add-post-validators
  [fn-map validators]
  (util/safe-update-in fn-map [:prepost-map :post] into validators))

(defn update-with-validation
  [fn-map]
  (if-not (add-validation? fn-map)
    fn-map
    (let [glossary (util/safe-get fn-map :glossary)
          _ (assert (ifn? glossary) "Glossary must be a function")
          input-kws (util/safe-get fn-map :input-kws)
          pre-validators (doall (mapcat #(make-validator % glossary true)
                                        input-kws))
          post-validators (when (contains? fn-map :output)
                            (make-validator (:output fn-map) glossary false))]
      (-> fn-map
          (add-pre-validators pre-validators)
          (add-post-validators post-validators)))))

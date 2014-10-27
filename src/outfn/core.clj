(ns outfn.core
  (:require [outfn.analyze :as analyze]
            [outfn.generate :as generate]
            [outfn.glossary :as glossary]
            [outfn.util :as util]))

(defn defoutfn*
  "TODO"
  [outfn-name {:keys [glossary] :as params} docstring raw-forms]
  {:pre [(symbol? outfn-name)
         (string? docstring)
         (ifn? glossary)]}
  (let [forms (if (list? (first raw-forms))
                raw-forms
                (list raw-forms))
        outfn-var (eval `(declare ~outfn-name))
        common-data-map (util/safe-merge params
                                         {:ns *ns*
                                          :outfn-name outfn-name
                                          :outfn-var outfn-var})]
    (->> forms
         analyze/read-fns
         (mapv util/safe-merge (repeat common-data-map))
         (mapv glossary/update-with-validation)
         (mapv generate/generate-fn!))
    `(defmacro ~outfn-name
       ~docstring
       [& {:as arg-map#}]
       (assert (every? keyword? (keys arg-map#))
               (format (str "The first of every pair of arguments "
                            "to %s must be a keyword")
                       '~outfn-name))
       (generate/generated-fn-call ~outfn-var arg-map#))))

(defmacro defoutfn
  "TODO docstring"
  [outfn-name params-quoted docstring & forms]
  (defoutfn* outfn-name (eval params-quoted) docstring forms))

;; -----------------
;; sample glossaries
;; -----------------

(defn no-glossary
  "An empty glossary with no information; doesn't provide any safety"
  [kw]
  {})

(defn base-glossary
  "TODO docstring"
  [kw]
  (let [arg (name kw)]
    (cond
     ;; vars ending with ? should be boolean
     (= \? (last arg)) {:validator #(or (true? %) (false? %))}
     ;; ignore small variables, they shouldn't have concepts attached
     (< (count arg) 3) {})))

(ns outfn.core
  (:require [outfn.analyze :as analyze]
            [outfn.generate :as generate]
            [outfn.glossary :as glossary]
            [outfn.state :as state]
            [outfn.util :as util]
            [slingshot.slingshot :as ss]))

(defn defoutfn*
  "TODO"
  [outfn-name params docstring raw-forms]
  {:pre [(symbol? outfn-name)
         (string? docstring)]}
  (ss/try+
   (let [forms (if (list? (first raw-forms))
                 raw-forms
                 (list raw-forms))
         outfn-var (eval `(declare ~outfn-name))
         common-data-map (util/safe-merge params
                                          {:ns *ns*
                                           :outfn-name outfn-name
                                           :outfn-var outfn-var})]
     (state/clear-var! outfn-var)
     (state/save-common-data! outfn-var common-data-map)
     (->> forms
          analyze/read-fns
          (mapv util/safe-merge (repeat common-data-map))
          (mapv glossary/update-with-validation)
          (mapv generate/generate-fn!))
     `(defmacro ~outfn-name
        ~docstring
        [& {:as arg-map#}]
        (ss/try+
         (assert (every? keyword? (keys arg-map#))
                 (format (str "The first of every pair of arguments "
                              "to %s must be a keyword")
                         '~outfn-name))
         (generate/generated-fn-call ~outfn-var (or arg-map# {}))
         (catch Object e#
           (ss/throw+ {:thrown e#
                       :error-time :macroexpand
                       :outfn-name '~outfn-name
                       :arg-map arg-map#})))))
   (catch Object e
     (ss/throw+ {:thrown e
                 :error-time :macroexpand
                 :outfn-name outfn-name}))))

(defmacro defoutfn
  "TODO docstring"
  [outfn-name & fdecl]
  (let [[params-quoted fdecl] (if (string? (first fdecl))
                                [nil fdecl]
                                [(first fdecl) (rest fdecl)])
        [docstring forms] [(first fdecl) (rest fdecl)]]
    (defoutfn* outfn-name (eval params-quoted) docstring forms)))

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

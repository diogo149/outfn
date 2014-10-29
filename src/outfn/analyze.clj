(ns outfn.analyze)

(defn to-input-keys
  [args]
  {:pre [(every? symbol? args)]}
  (set (map keyword args)))

(defn read-fn
  [[raw-args & raw-fn-body]]
  {:pre [(vector? raw-args)
         (every? symbol? raw-args)
         (not-any? #{'&} raw-args)]}
  (let [arg-set (set raw-args)
        input-kws (to-input-keys raw-args)
        non-empty-fn-body (or raw-fn-body '(nil))
        [raw-prepost-map fn-body] (cond
                                   ;; make fn-body return nil for empty functions
                                   ;; so that a prepost map isn't returned
                                   (empty? raw-fn-body)
                                   [{} '(nil)]

                                   ;; parse out the prepost-map, if there is one
                                   (and (> (count raw-fn-body) 1)
                                        (->> raw-fn-body first map?))
                                   [(first raw-fn-body) (rest raw-fn-body)]

                                   :else
                                   [{} raw-fn-body])
        prepost-map (-> raw-prepost-map
                        (update-in [:pre] #(or % []))
                        (update-in [:post] #(or % [])))]
    (assert (= (count raw-args) (count arg-set))
            (format "Non unique function argument in: %s" raw-args))
    (assert (and (vector? (:pre prepost-map))
                 (vector? (:post prepost-map)))
            (format "Prepost-map %s doesn't have vector for :pre or :post"
                    prepost-map))
    {:arg-set arg-set
     :input-kws input-kws
     :prepost-map prepost-map
     :fn-body fn-body}))

(defn read-fns
  [forms]
  (let [fn-maps (mapv read-fn forms)]
    (assert (= (count fn-maps)
               (->> fn-maps
                    (map :arg-set)
                    distinct
                    count))
            "Function with duplicate keys found")
    fn-maps))

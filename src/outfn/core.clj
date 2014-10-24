(ns outfn.core)

(defn make-generated-fn-name
  [outfn-name arg-set]
  (->> arg-set
       sort
       (cons "outfn")
       (cons outfn-name)
       (interpose "-")
       clojure.string/join
       symbol))

(defn make-dispatch-map-name
  [outfn-name]
  (symbol (str outfn-name "-outfn-dispatch-fn")))

;; ---------------------------------------------
;; TODO add this section to a glossary namespace
;; ---------------------------------------------

(defn add-validators?
  []
  ;; TODO some logic of when to and not to create validation for every call
  true)

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

(defn make-validator
  [glossary arg-name]
  (let [glossary-entry (->> arg-name keyword glossary)
        {:keys [validator schema]} glossary-entry]
    (assert glossary-entry (format "Invalid key %s: not in provided glossary"
                                   arg-name))
    [(when validator
       (list validator arg-name))
     (when schema
       ;; TODO schema validate
       )]))

;; ----------------------
;; end glossary namespace
;; ----------------------

(defn get-arg-set
  "takes in the body of a function and returns the arguments to that function
  as a sorted set
  eg. (get-arg-set '([foo bar choo] (+ foo bar))) => #{bar choo foo}"
  [[args]]
  {:pre [(vector? args)
         (every? symbol? args)
         (not-any? #{'&} args)]
   :post [(= (count %) (count args))]}
  (into (sorted-set) args))

(defn make-fn-impl
  [glossary fn-name fn-arg-set fn-body]
  (let [;; discard the arg set, since we need to provide a sorted vector
        ;; so that we need what order the positional arguments should be in
        fn-body (rest fn-body)
        ;; make fn-body return nil for empty functions
        fn-body (or fn-body '(nil))
        [prepost-map fn-body] (if (and (> (count fn-body) 1)
                                       (->> fn-body first map?))
                                [(first fn-body) (rest fn-body)]
                                [{} fn-body])
        validators (->> fn-arg-set
                        (mapcat #(make-validator glossary %))
                        (remove nil?))
        ;; make sure prepost-map has a vector
        prepost-map (update-in prepost-map [:pre] #(or % []))
        _ (assert (vector? (:pre prepost-map))
                  (format "Prepost-map %s doesn't have vector for :pre"
                          prepost-map))
        prepost-map (if-not (and (add-validators?) (seq validators))
                      prepost-map
                      (update-in prepost-map
                                 [:pre]
                                 (fn [x] (into x validators))))
        res `(defn ~fn-name
               ~(vec (sort fn-arg-set))
               ~prepost-map
               ~@fn-body)]
    (prn res) ;; TODO kill this line
    res))

(defn make-dispatch-map
  [fn-arg-sets fn-names]
  (let [fn-key-sets (map #(->> % (map keyword) set) fn-arg-sets)]
    (zipmap fn-key-sets fn-names)))

(defmacro defoutfn
  "TODO docstring"
  [outfn-name params-quoted docstring & forms]
  {:pre [(symbol? outfn-name)
         (string? docstring)]}
  (let [{:keys [glossary] :as params} (eval params-quoted)
        _ (assert (ifn? glossary) (format "(:glossary %s) needs to be a function"
                                          params-quoted))
        fn-bodies (if (list? (first forms))
                    forms
                    (list forms))
        fn-arg-sets (distinct (map get-arg-set fn-bodies))
        ;; making sure arg-sets are unique
        _ (assert (= (count fn-arg-sets) (count fn-bodies)))
        fn-names (map #(make-generated-fn-name outfn-name %) fn-arg-sets)
        defn-impls (map (partial make-fn-impl glossary)
                        fn-names
                        fn-arg-sets
                        fn-bodies)
        dispatch-map-name (make-dispatch-map-name outfn-name)
        dispatch-map-doc (format (str "Map for outfn %s from set of keys"
                                      " to function name")
                                 outfn-name)
        dispatch-map (make-dispatch-map fn-arg-sets fn-names)
        ;; bind namespace to qualify the dispatched function call in the
        ;; inner macro
        outfn-ns *ns*]
    `(do
       (def ~dispatch-map-name ~dispatch-map-doc '~dispatch-map)
       (println ~dispatch-map-name)
       ~@defn-impls
       (defmacro ~outfn-name
         ~docstring
         [& {:as args#}]
         (let [k# (into (sorted-set) (keys args#))
               _# (assert (every? keyword? k#)
                          (format (str "The first of every pair of arguments "
                                       "to %s must be a keyword: %s")
                                  '~outfn-name
                                  k#))
               dispatch-fn-name# (~dispatch-map-name k#)
               _# (assert dispatch-fn-name#
                          (format "Invalid set of keys %s for %s"
                                  k#
                                  '~outfn-name))
               ;; TODO is there a better way to namespace qualify this function
               dispatched-fn# (symbol (str ~outfn-ns) (str dispatch-fn-name#))
               positional-args# (map args# k#)]
           (cons dispatched-fn# positional-args#))))))

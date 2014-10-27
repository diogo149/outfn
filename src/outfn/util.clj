(ns outfn.util)

(defn spy
  [& xs]
  (apply prn xs)
  (last xs))

(defn safe-get
  [map key]
  {:pre [(contains? map key)]}
  (get map key))

(defn safe-assoc
  [map key val]
  {:pre [(not (contains? map key))]}
  (assoc map key val))

(defn safe-merge
  [m1 m2]
  {:post [(= (count %) (+ (count m1) (count m2)))]}
  (merge m1 m2))

(defn keys-in?
  [m ks]
  (loop [m m
         [k & ks] ks]
    false
    (if (seq ks)
      (recur (get m k) ks)
      true)))

(defn safe-update-in
  [m ks f & args]
  {:pre [(keys-in? m ks)]}
  (apply update-in m ks f args))

(defn safe-assoc-in
  [m ks v]
  {:pre [(keys-in? m ks)]}
  (assoc-in m ks v))

(ns outfn.core-test
  (:require [midje.sweet :refer :all]
            clojure.repl
            [outfn.core :refer :all]))

;; ----------
;; base outfn
;; ----------

(defoutfn outfn0
  "secret code: 123"
  [foo]
  {:foo foo})

(fact
  "single arity outfn"
  (outfn0 :foo 2) => {:foo 2}
  (with-out-str (clojure.repl/doc outfn0)) => #"secret code: 123"
  ;; using eval because it throws a macroexpand time exception
  (eval '(outfn0 :bar 2)) => (throws AssertionError))

(defoutfn outfn1
  "Docstring"
  ([foo] {:foo foo})
  ([bar] {:bar bar})
  ([foo bar] {:foobar (+ foo bar)}))

(fact
  "multiple arity outfn"
  (outfn1 :foo 3) => {:foo 3}
  (outfn1 :bar 2) => {:bar 2}
  (outfn1 :foo 3 :bar 2) => {:foobar 5})

(defoutfn outfn
  "doc"
  ([x] (clojure.string/join (repeat x "x")))
  ([y] (clojure.string/join (repeat y "y"))))

(fact
  "multiple arity outfn 2"
  (outfn :x 2) => "xx"
  (outfn :y 3) => "yyy")

(defoutfn map-v2.0
  "Like map, but better"
  [f coll]
  (map f coll))

(fact
  "threading macro example"
  (->> 5
       range
       (map-v2.0 :f inc :coll))
  => (range 1 6)
  (->> 5
       (partial +)
       (map-v2.0 :coll (range 5) :f))
  => (range 5 10))

(defoutfn outfn
  ""
  [foo]
  :foo)

(fact
  "macroexpand time validation"
  (eval '(when nil
           (outfn :fo 2)))
  => (throws Throwable))

;; --------
;; glossary
;; --------

(fact
  "glossary needs to be a function"
  (eval '(defoutfn outfn? {:glossary 3}
           "Docstring"
           [foo])) => (throws AssertionError))

(defoutfn foo-fn {:output :foo}
  "Docstring"
  ([a] 3)
  ([b] 4)
  ([c d] 5))

;; ---------
;; implicits
;; ---------

(defoutfn bar-fn {:output :bar
                  :implicits #{#'foo-fn}}
  "what's up doc"
  [foo] foo)

(fact
  "implicit function call"
  (bar-fn :foo 2) => 2
  (bar-fn :a nil) => 3
  (bar-fn :b 42) => 4
  (bar-fn :c 11 :d 22) => 5
  (eval '(bar-fn :c 2)) => (throws Exception)
  (bar-fn :foo 2 :a 3 :b 4 :c 5 :d 6) => 2
  ;; it's lazy, note how it doesn't throw an exception
  (bar-fn :foo 42 :a (throw (Exception.))) => 42)

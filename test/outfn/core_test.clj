(ns outfn.core-test
  (:require [midje.sweet :refer :all]
            clojure.repl
            [outfn.core :refer :all]))

(defoutfn outfn0 {:glossary no-glossary}
  "secret code: 123"
  [foo]
  nil)

(fact
  "single arity outfn"
  (outfn0 :foo 2) => nil
  (with-out-str (clojure.repl/doc outfn0)) => #"secret code: 123"
  ;; using eval because it throws a macroexpand time exception
  (eval '(outfn0 :bar 2)) => (throws AssertionError))

(defoutfn outfn1 {:glossary no-glossary}
  "Docstring"
  ([foo] {:foo foo})
  ([bar] {:bar bar})
  ([foo bar] {:foobar (+ foo bar)}))

(fact
  "multiple arity outfn"
  (outfn1 :foo 3) => {:foo 3}
  (outfn1 :bar 2) => {:bar 2}
  (outfn1 :foo 3 :bar 2) => {:foobar 5})

(fact
  "glossary needs to be a function"
  (eval '(defoutfn outfn? {}
           "Docstring"
           [foo])) => (throws AssertionError))

(defoutfn foo-fn {:glossary no-glossary
                  :output :foo}
  "Docstring"
  ([a] 3)
  ([b] 4)
  ([c d] 5))

(defoutfn bar-fn {:glossary no-glossary
                  :output :bar
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

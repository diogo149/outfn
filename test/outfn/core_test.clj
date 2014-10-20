(ns outfn.core-test
  (:require [midje.sweet :refer :all]
            [outfn.core :refer :all]))

(defoutfn outfn0 {:glossary no-glossary}
  ""
  [foo]
  nil)

(fact "testing single arity outfn"
  (outfn0 :foo 2) => nil
  ;; using eval because it throws a macroexpand time exception
  (eval '(outfn0 :bar 2)) => (throws AssertionError))

(defoutfn outfn1 {:glossary no-glossary}
  "Docstring"
  ([foo] {:foo foo})
  ([bar] {:bar bar})
  ([foo bar] {:foobar (+ foo bar)}))

(fact "testing multiple arity outfn"
  (outfn1 :foo 3) => {:foo 3}
  (outfn1 :bar 2) => {:bar 2}
  (outfn1 :foo 3 :bar 2) => {:foobar 5})

(fact "testing that glossary needs to be a function"
  (eval '(defoutfn outfn? {}
           "Docstring"
           [foo])) => (throws AssertionError))

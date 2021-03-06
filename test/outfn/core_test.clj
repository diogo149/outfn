(ns outfn.core-test
  (:require [midje.sweet :refer :all]
            clojure.repl
            [schema.core :as s]
            [slingshot.slingshot :as ss]
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
  (eval '(outfn0 :bar 2)) => (throws Exception))

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

(fact
  "prefer exact match when possible"
  (outfn1 :foo 3 :bar 2) => {:foobar 5})

(fact
  "take first match when no exact match"
  (outfn1 :bar 2 :foo 3 :choo 18) => {:foo 3})

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
  => (throws Exception))

;; --------
;; glossary
;; --------

(fact
  "glossary needs to be a function"
  (eval '(defoutfn outfn? {:glossary 3}
           "Docstring"
           [foo])) => (throws Exception))

(def a-glossary {:foo {:validator #(= :foo %)}
                 :choo {:schema {:a {:b s/Int}}}
                 :bar {:validator keyword?}})

(defoutfn outfn1 {:glossary a-glossary
                  :output :bar}
  "Some outfn"
  ([foo] 3.5)
  ([choo] :lochness))

(defoutfn outfn2 {:glossary a-glossary
                  :output :bar}
  "Some outfn"
  ([foo] :hello)
  ([choo] :world))

(fact
  "DRY validation"
  (outfn1 :foo :foo) => (throws Exception) ;; output validation
  (outfn1 :foo 42) => (throws Exception)
  (outfn1 :choo {:a {:b 3}}) => :lochness
  (outfn1 :choo {:a {:b "3"}}) => (throws Exception)
  (outfn2 :foo :foo) => :hello
  (outfn2 :foo :bar) => (throws Exception)
  (outfn2 :choo {:a {:b 3}}) => :world
  (outfn2 :choo {:a {:b "3"}}) => (throws Exception))

;; ---------
;; implicits
;; ---------

(defoutfn foo-fn {:output :foo}
  "Docstring"
  ([a] 3)
  ([b] 4)
  ([c d] 5))

(defoutfn bar-fn {:implicits #{#'foo-fn}}
  "what's up doc"
  [foo] foo)

(fact
  "implicit function call"
  (bar-fn :foo 2) => 2
  (bar-fn :a nil) => 3
  (bar-fn :b 42) => 4
  (bar-fn :c 11 :d 22) => 5
  (eval '(bar-fn :c 2)) => (throws Exception)
  (bar-fn :foo 2 :a 3 :b 4 :c 5 :d 6 :q 72) => 2
  ;; it's lazy, note how it doesn't throw an exception
  (bar-fn :foo 42 :a (throw (Exception.))) => 42)

(fact
  "macroexpand-time implicit validation"
  (eval '(when nil (bar-fn :q 11))) => (throws Exception))

(def big-let-block (let [a 42
                         b (+ 31 a)
                         c (* 2 b)
                         d (+ a b)
                         e (/ c a)
                         f (* d e)
                         g (dec e)]
                     (+ b d f)))

(declare bar-fn)
(defoutfn foo-fn {:output :foo
                  :implicits #{#'bar-fn}}
  "docstring"
  []
  :foo)

(defoutfn bar-fn {:output :bar
                  :implicits #{#'foo-fn}}
  "docstring"
  [foo]
  foo)

(fact
  "implicits with 0 argument outfns"
  (bar-fn) => :foo
  (foo-fn) => :foo
  (foo-fn :bar 3) => :foo)

(defoutfn a {:output :a} "Returns an a" [] 42)
(defoutfn b {:output :b} "Returns a b" [a] (+ 31 a))
(defoutfn c {:output :c} "Returns a c" [b] (* 2 b))
(defoutfn d {:output :d} "Returns a d" [a b] (+ a b))
(defoutfn e {:output :e} "Returns an e" [a c] (/ c a))
(defoutfn f {:output :f} "Returns a f" [d e] (* d e))
(defoutfn g {:output :g} "Returns a g" [e] (dec e))
(defoutfn result {:output :result
                  :implicits #{#'a #'b #'c #'d #'e #'f #'g}}
  "Returns the answer!"
  [b d f]
  (+ b d f))

(fact
  "solving big let block problem"
  (result) => big-let-block)

(def big-let-block-2 (let [a 42
                           b (+ a 16)
                           c (* a b)]
                       (+ a b c)))

(defoutfn a {:output :a} "Returns an a" [] 42)
(defoutfn b {:output :b} "Returns a b" [a] (+ a 16))
(defoutfn c {:output :c} "Returns a c" [a b] (* a b))
(defoutfn result {:output :result
                  :implicits #{#'a #'b #'c}}
  "Returns the answer"
  [a b c]
  (+ a b c))
(fact
  "big let problem 2"
  (result) => big-let-block-2)

;; ----------------------
;; implicits + validation
;; ----------------------

(def a-glossary {:q {:validator odd?}
                 :a {}})

(defoutfn a {:output :a
             :glossary a-glossary}
  "Returns an a"
  [q]
  q)
(defoutfn b {:output :b :implicits #{#'a}} "Returns a b" [a] a)
(fact
  "validation with implicits"
  (a :q 3) => 3
  (a :q 0) => (throws Exception))

;; --------------------------
;; implicits + error handling
;; --------------------------

(defoutfn a {:output :a} "Returns an a" [] (assert false) 42)
(defoutfn b {:output :b :implicits #{#'a}} "Returns a b" [a] (+ a 16))
(fact
  "Implicits error handling"
  (ss/try+ (b) (catch Object e e))
  => (contains {:error-time :runtime
                :outfn-var #'b
                :computation-order [[:a #{}] [:b #{:a}]]
                :computation-step [:a #{}]
                :intermediate-var #'a}))

(defoutfn a {:output :a} "Returns an a" [] 42)
(defoutfn b {:output :b} "Returns a b" [a] (assert false) (+ a 16))
(defoutfn c {:output :c :implicits #{#'a #'b}} "Returns a c" [a b] (* a b))
(fact
  "Implicits error handling 2"
  (ss/try+ (c) (catch Object e e))
  => (contains {:error-time :runtime
                :outfn-var #'c
                :computation-order [[:a #{}] [:b #{:a}] [:c #{:a :b}]]
                :computation-step [:b #{:a}]
                :intermediate-var #'b}))

(defoutfn a {:output :a} "Returns an a" [] (map (fn [x] (assert false)) [1]))
(defoutfn b {:output :b} "Returns a b" [a] (+ a 16))
(defoutfn c {:output :c :implicits #{#'a #'b}} "Returns a c" [a b] (* a b))
(fact
  "Implicits error handling w/ lazy intermediate and not evaluating it"
  (ss/try+ (c) (catch Object e e))
  => (contains {:error-time :runtime
                :outfn-var #'c
                :computation-order [[:a #{}] [:b #{:a}] [:c #{:a :b}]]
                :computation-step [:b #{:a}]
                :intermediate-var #'b}))

(defoutfn a {:output :a} "Returns an a" [] (map (fn [x] (assert false)) [1]))
(defoutfn b {:output :b} "Returns a b" [a] (dorun a))
(defoutfn c {:output :c :implicits #{#'a #'b}} "Returns a c" [a b] (* a b))
(fact
  "Implicits error handling w/ lazy intermediate and evaluating it"
  (ss/try+ (c) (catch Object e e))
  => (contains {:error-time :runtime
                :outfn-var #'c
                :computation-order [[:a #{}] [:b #{:a}] [:c #{:a :b}]]
                :computation-step [:b #{:a}]
                :intermediate-var #'b}))

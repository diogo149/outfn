(ns outfn.glossary-test
  (:require [outfn.glossary :refer :all]
            [midje.sweet :refer :all]
            [schema.core :as s]))

(fact
  "testing make-validator"
  (make-validator :bar {:bar {:validator keyword?}} false)
  => `((~keyword? ~'%)))

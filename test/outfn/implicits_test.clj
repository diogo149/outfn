(ns outfn.implicits-test
  (:require [outfn.implicits :refer :all]
            [midje.sweet :refer :all]
            [loom.graph :as loom]))

(fact
  "preprocess-computation"
  (let [computation {:inputs #{:foo}
                     :output :bar
                     :cost 3}]
    (preprocess-computation computation)
    => computation
    (preprocess-computation (dissoc computation :cost))
    => (assoc computation :cost 1)))

(tabular
 (fact
   "greedy-compute-implicits"
   (greedy-compute-implicits input-kws output-kw available-computations)
   => result)
 input-kws output-kw available-computations result

 #{:foo} :bar [{:inputs #{:foo}
                :cost 3
                :output :bar}]
 (loom/digraph [:foo :bar])

 #{:foo :bar} :bar [{:inputs #{:foo}
                     :cost 3
                     :output :bar}]
 (loom/digraph :foo :bar)

 #{:foo :choo :doo :baz} :bar [{:inputs #{:foo}
                                :cost 100
                                :output :bar}
                               {:inputs #{:choo :doo :baz}
                                :cost 99
                                :output :bar}]
 (loom/digraph :foo [:choo :bar] [:doo :bar] [:baz :bar])

 #{:foo :choo} :bar [{:inputs #{:foo}
                      :cost 100
                      :output :bar}
                     {:inputs #{:choo}
                      :cost 1
                      :output :doo}
                     {:inputs #{:doo}
                      :cost 1
                      :output :baz}
                     {:inputs #{:baz}
                      :cost 1
                      :output :bar}]
 (loom/digraph :foo [:choo :doo] [:doo :baz] [:baz :bar])

 #{:foo :choo} :bar [{:inputs #{:foo}
                      :cost 2
                      :output :bar}
                     {:inputs #{:choo}
                      :cost 1
                      :output :doo}
                     {:inputs #{:doo}
                      :cost 1
                      :output :baz}
                     {:inputs #{:baz}
                      :cost 1
                      :output :bar}]
 (loom/digraph [:foo :bar] :choo))

(tabular
 (fact
   "generate serial order"
   (generate-serial-order input-kws output-kw graph) => result)
 input-kws output-kw graph result

 #{:bar} :foo (loom/digraph [:bar :foo])
 [[:foo #{:bar}]]

 #{:a :b} :q (loom/digraph [:a :c] [:b :c] [:c :d] [:d :e] [:e :q])
 [[:c #{:a :b}] [:d #{:c}] [:e #{:d}] [:q #{:e}]])

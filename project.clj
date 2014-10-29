(defproject outfn "0.1.0"
  :description "A macro for clearer, more declarative functions."
  :url "https://github.com/diogo149/outfn"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[aysylu/loom "0.5.0"]
                 [prismatic/schema "0.3.1"]
                 [slingshot "0.12.1"]]
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.6.0"]
                                  [midje "1.6.3"]]
                   :plugins [[lein-midje "3.1.3"]]
                   :repl-options {:init-ns outfn.core}}})

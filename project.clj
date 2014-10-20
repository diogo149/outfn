(defproject outfn "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies []
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.6.0"]
                                  [midje "1.6.3"]]
                   :plugins [[lein-midje "3.1.3"]]
                   :repl-options {:init-ns outfn.core}}})

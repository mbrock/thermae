(defproject thermae "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "AGPL-3.0-or-later"
            :url "https://www.gnu.org/licenses/agpl-3.0.en.html"}
  :dependencies [
                 [org.clojure/clojure "1.10.0"]
                 [org.clojure/core.match "1.0.0"]
                 [datascript "1.0.7"]
                 [hiccup "1.0.5"]
                 [stasis "2.5.0"]
                 [ring "1.9.1"]
                 [slugger "1.0.1"]
                 [instaparse "1.4.10"]
                 ]
  :plugins [[lein-ring "0.12.5"]]
  :ring {:handler thermae.core/app}
  :main ^:skip-aot thermae.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

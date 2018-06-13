(defproject asparapiss "0.1.0-SNAPSHOT"
  :description
  "Plot lines that fit points"
  :url "http://geokon-gh.github.io/asparapiss/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot asparapiss.core
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [net.mikera/core.matrix "0.62.0"]
                 [net.mikera/vectorz-clj "0.47.0"]
                 [halgari/fn-fx "0.4.0"]
                 [com.github.afester.javafx/FranzXaver "0.1"]
                 [thi.ng/geom-viz "0.0.908"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

(defproject devs "0.1.0"
  :description "Discrete Event System Simulation - a generic state machine"
  :url "http://github.com/mtnygard/devs"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.async "0.1.0-SNAPSHOT"]]
  :profiles {:dev {:dependencies [[expectations "1.4.36"]]}}
  :plugins [[lein-autoexpect "0.1.2"]])

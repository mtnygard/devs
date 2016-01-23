(defproject devs "0.2.0-SNAPSHOT"
  :description "Discrete Event System Simulation - a generic state machine"
  :url "http://github.com/mtnygard/devs"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.374"]]
  :profiles {:dev {:dependencies [[expectations "2.1.4"]]}}
  :plugins [[lein-autoexpect "1.7.0"]])

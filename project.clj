(defproject devs "0.2.1"
  :description "Discrete Event System Simulation - a generic state machine"
  :url "http://github.com/mtnygard/devs"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.async "0.4.474"]]
  :profiles {:dev {:dependencies [[expectations "2.1.4"]]}}
  :plugins [[lein-autoexpect "1.7.0"]])

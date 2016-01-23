(ns devs.core-test
  (:require [devs.core :refer :all]
            [expectations :refer :all]
            [clojure.core.async :as a]))

;;; new style of interface, fewer functions more data
(def m1a
  (-> {:input-alphabet #{:evt-a :evt-b :evt-c}
       :state-alphabet #{:waiting-for-a :waiting-for-b :waiting-for-c :waiting-for-godot}
       :state          :waiting-for-a}
      (on :waiting-for-a :evt-a :waiting-for-b)
      (on :waiting-for-b :evt-b :waiting-for-c)
      (on :waiting-for-c :evt-c :waiting-for-godot)))

(expect {:state :waiting-for-b}     (in (evolve m1a :evt-a)))
(expect {:state :waiting-for-c}     (in (evolve (evolve m1a :evt-a) :evt-b)))
(expect {:state :waiting-for-godot} (in (evolve (evolve (evolve m1a :evt-a) :evt-b) :evt-c)))
(expect clojure.lang.ExceptionInfo  (in (evolve m1a :godot)))

;;; internal events for automatic state transitions
(def m2
  (-> {:input-alphabet #{:evt-a :evt-b :evt-c}
       :state-alphabet #{:waiting-for-a :waiting-for-b :waiting-for-c :waiting-for-godot}
       :state          :waiting-for-a}
      (on :waiting-for-a :evt-a :waiting-for-b (automatic :evt-b))
      (on :waiting-for-b :evt-b :waiting-for-c (automatic :evt-c))
      (on :waiting-for-c :evt-c :waiting-for-godot)))

(expect {:state :waiting-for-godot} (in (evolve m2 :evt-a)))

(def pinger
  (-> {:input-alphabet #{:ping :quit :ponged}
       :state-alphabet #{:receiving :sending :closed}
       :output-alphabet #{:pong :done}
       :state          :receiving}
      (on :receiving :ping :sending (automatic :ponged))
      (on :receiving :quit :closed)
      (on :sending :quit :closed)
      (on :sending :ponged :receiving)
      (on :closed :quit :closed)
      (outputs :sending :pong)
      (outputs :closed  :done)))

(def pinger'  (evolve pinger  :ping))
(def pinger'' (evolve pinger' :ping))

(expect '(:pong) (:output pinger'))
(expect '(:pong :pong) (:output pinger''))

(def broken-pinger
  (-> {:input-alphabet  #{:ping}
       :state-alphabet  #{:receiving :sending}
       :state           :receiving
       :pinger-working? false}
      (on :receiving :ping :sending (guard :pinger-working?))))

(def broken-pinger' (evolve broken-pinger :ping))
(def fixed-pinger   (evolve (assoc broken-pinger :pinger-working? true) :ping))

(expect {:state :receiving} (in broken-pinger'))
(expect {:state :sending}   (in fixed-pinger))

(defn run-machine
  [machine inputs]
  (let [input-chan  (a/chan)
        [out error] (evolve! machine input-chan)]
    (a/go
     (doseq [in inputs]
       (a/>! input-chan in)))
    (loop [outputs []
           machine-states []]
      (let [[v c] (a/alts!! [out error (a/timeout 500)] :priority true)]
        (cond
         (= out c)    (do
                        (if (= :done (first v))
                          [(conj outputs (first v)) (conj machine-states (second v))]
                          (recur (conj outputs (first v)) (conj machine-states (second v)))))

         (= error c)  (throw (ex-info (str "Error generated from machine: " v) {}))

         :else        (throw (ex-info "Test timed out before machine finished" {:outputs outputs :states machine-states})))))))

(expect [:pong :done]     (first (run-machine pinger [:ping :quit])))
(expect {:state :closed}  (in (last (second (run-machine pinger [:ping :quit])))))

(def ten-pings [:ping :ping :ping :ping :ping :ping :ping :ping :ping :ping :quit])
(def ten-pongs [:pong :pong :pong :pong :pong :pong :pong :pong :pong :pong :done])

(expect ten-pongs         (first (run-machine pinger ten-pings)))
(expect {:state :closed}  (in (last (second (run-machine pinger ten-pings)))))

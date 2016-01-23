(ns devs.core-test
  (:require [devs.core :refer :all]
            [expectations :refer :all]
            [clojure.core.async :as a]))

;;; new style of interface, fewer functions more data
(def machine1
  (-> {:input-alphabet #{:event-a :event-b :event-c}
       :state-alphabet #{:waiting-for-a :waiting-for-b :waiting-for-c :waiting-for-final}
       :state          :waiting-for-a}
      (on :waiting-for-a :event-a :waiting-for-b)
      (on :waiting-for-b :event-b :waiting-for-c)
      (on :waiting-for-c :event-c :waiting-for-final)))

(expect {:state :waiting-for-b}     (in (evolve machine1 :event-a)))
(expect {:state :waiting-for-c}     (in (evolve (evolve machine1 :event-a) :event-b)))
(expect {:state :waiting-for-final} (in (evolve (evolve (evolve machine1 :event-a) :event-b) :event-c)))
(expect clojure.lang.ExceptionInfo  (in (evolve machine1 :final)))

;;; internal events for automatic state transitions
(def machine2
  (-> {:input-alphabet #{:event-a :event-b :event-c}
       :state-alphabet #{:waiting-for-a :waiting-for-b :waiting-for-c :waiting-for-final}
       :state          :waiting-for-a}
      (on :waiting-for-a :event-a :waiting-for-b (automatic :event-b))
      (on :waiting-for-b :event-b :waiting-for-c (automatic :event-c))
      (on :waiting-for-c :event-c :waiting-for-final)))

(expect {:state :waiting-for-final} (in (evolve machine2 :event-a)))

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
  (let [input-chan (a/chan)
        out-chan   (a/chan)
        err-chan   (a/chan (a/dropping-buffer 1000))
        terminate  (evolve! machine input-chan out-chan err-chan)]
    (a/go
      (doseq [in inputs]
        (a/>! input-chan in)))
    (loop [outputs        []
           machine-states []]
      (let [[v c] (a/alts!! [out-chan err-chan (a/timeout 500)] :priority true)]
        (cond
          (= out-chan c) (do
                           (if (= :done (first v))
                             [(conj outputs (first v)) (conj machine-states (second v))]
                             (recur (conj outputs (first v)) (conj machine-states (second v)))))
          (= err-chan c) (throw (ex-info (str "Error generated from machine: " v) {}))
          :else          (throw (ex-info "Test timed out before machine finished" {:outputs outputs :states machine-states})))))))

(expect [:pong :done]     (first (run-machine pinger [:ping :quit])))
(expect {:state :closed}  (in (last (second (run-machine pinger [:ping :quit])))))

(def ten-pings [:ping :ping :ping :ping :ping :ping :ping :ping :ping :ping :quit])
(def ten-pongs [:pong :pong :pong :pong :pong :pong :pong :pong :pong :pong :done])

(expect ten-pongs         (first (run-machine pinger ten-pings)))
(expect {:state :closed}  (in (last (second (run-machine pinger ten-pings)))))

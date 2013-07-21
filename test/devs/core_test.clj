(ns devs.core-test
  (:require [devs.core :refer :all]
            [expectations :refer :all]
            [clojure.core.async :refer [chan >! <!! go timeout alts!! close!]]))

;;; in-state? returns a predicate that returns the whole machine on a match, nil otherwise
(expect {:state :initial} ((in-state? :initial) {:state :initial}))
(expect nil               ((in-state? :initial) {:state :anything-else}))
(expect {:state :initial :extra-key 'with-data} (in ((in-state? :initial) {:state :initial :extra-key 'with-data})))

;;; side-effect returns a function that calls a function. whatever the function returns becomes the new state
(expect 'foo              ((side-effect (fn [_] 'foo)) {:state :anything}))
(expect {:state :next
         :extra 'foo}     (in ((side-effect (fn [state]
                                              (merge state
                                                     {:state :next
                                                      :extra 'foo})))
                               {:state :anything})))

;;; new-state returns a function that, when applied, will update the state machine's state
(expect :listening (:state ((new-state :listening) {:state :initial})))

;;; on-event attaches a new transition to the transition map
(expect true (contains? (on-event {:input-alphabet #{:read :write}} :read identity) :transitions))
(expect true (seq? (get-in 
                    (on-event {:input-alphabet #{:read :write}} :read identity)
                    [:transitions :read])))
(expect false (seq? (get-in
                     (on-event {:input-alphabet #{:read :write}} :read identity)
                     [:transitions :write])))

(def m1
  (-> {:input-alphabet #{:evt-a :evt-b :evt-c}
          :state-alphabet #{:waiting-for-a :waiting-for-b :waiting-for-c :waiting-for-godot}
       :state          :waiting-for-a}
      (on-event :evt-a (in-state? :waiting-for-a) (new-state :waiting-for-b))
      (on-event :evt-b (in-state? :waiting-for-b) (new-state :waiting-for-c))
      (on-event :evt-c (in-state? :waiting-for-c) (new-state :waiting-for-godot))
      (on-event :godot (new-state :waiting-for-godot))))
(expect :waiting-for-b (:state (evolve m1 :evt-a)))
(expect :waiting-for-c (:state (evolve (evolve m1 :evt-a) :evt-b)))
(expect :waiting-for-godot (:state (evolve (evolve (evolve m1 :evt-a) :evt-b) :evt-c)))
(expect :waiting-for-godot (:state (evolve m1 :godot)))
(expect :waiting-for-godot (:state (evolve (evolve (evolve m1 :godot) :godot) :godot)))
(expect :waiting-for-godot (:state (evolve (assoc m1 :state :waiting-c) :godot)))

;;; internal events for automatic state transitions
(def m2
  (-> {:input-alphabet #{:evt-a :evt-b :evt-c}
       :state-alphabet #{:waiting-for-a :waiting-for-b :waiting-for-c :waiting-for-godot}
       :state          :waiting-for-a}
      (on-event :evt-a (in-state? :waiting-for-a) (generate-event :evt-b) (new-state :waiting-for-b))
      (on-event :evt-b (in-state? :waiting-for-b) (new-state :waiting-for-c) (generate-event :evt-c))
      (on-event :evt-c (in-state? :waiting-for-c) (new-state :waiting-for-godot))
      (on-event :godot (new-state :waiting-for-godot))))
(expect :waiting-for-godot (:state (evolve m2 :evt-a)))

(def pinger
  (-> {:input-alphabet #{:ping :quit}
       :state-alphabet #{:receiving :sending :closed}
       :state          :receiving}
      (on-event :ping     (in-state? :receiving) (new-state :sending))
      (outputs  :sending  (generate :pong)       (new-state :receiving))
      (on-event :quit     (new-state :closed))
      (outputs  :closed   (generate :done))))

(def pinger'  (evolve pinger  :ping))
(def pinger'' (evolve pinger' :ping))
(expect '(:pong) (:output pinger'))
(expect '(:pong :pong) (:output pinger''))

(def broken-pinger
  (-> {:input-alphabet  #{:ping}
       :state-alphabet  #{:receiving :sending}
       :state           :receiving
       :pinger-working? false}
      (on-event :ping
                (in-state? :receiving)
                (guard :pinger-working? (new-state :sending)))))
(def broken-pinger' (evolve broken-pinger :ping))
(def fixed-pinger   (evolve (assoc broken-pinger :pinger-working? true) :ping))

(expect :receiving (:state broken-pinger'))
(expect :sending   (:state fixed-pinger))

(defn run-machine
  [machine inputs]
  (let [input-chan  (chan)
        [out error] (evolve! machine input-chan)]
    (go
     (doseq [in inputs] (>! input-chan in))
     (>! input-chan :quit))
    (loop [outputs []
           machine-states []]
      (let [[v c] (alts!! [out error (timeout 100)])]
        (cond
         (= out c)    (if (= :done (first v))
                        [(conj outputs (first v)) (conj machine-states (second v))]
                        (recur (conj outputs (first v)) (conj machine-states (second v))))

         (= error c)  (throw (ex-info (str "Error generated from machine: " v) {}))

         :else        (throw (ex-info "Test timed out before machine finished" {})))))))

(expect [:pong :done]                      (first (run-machine pinger [:ping])))
(expect {:state :receiving}                (in (first (second (run-machine pinger [:ping])))))

(expect (concat (repeat 10 :pong) [:done]) (first (run-machine pinger (repeat 10 :ping))))
(expect {:state :closed}                   (in (last (second (run-machine pinger (repeat 10 :ping))))))

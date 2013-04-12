(ns devs.core-test
  (:require [devs.core :refer :all]
            [expectations :refer :all])
  (:use clojure.pprint))

;;; in-state? returns a predicate that returns the whole machine on a match, nil otherwise
(expect {:state :initial} ((in-state? :initial) {:state :initial}))
(expect nil               ((in-state? :initial) {:state :anything-else}))
(expect {:state :initial :extra-key 'with-data} ((in-state? :initial) {:state :initial :extra-key 'with-data}))

;;; side-effect returns a function that calls a function. whatever the function returns becomes the new state
(expect 'foo              ((side-effect (fn [_] 'foo)) {:state :anything}))
(expect 'foo              ((side-effect (fn [_ extra] extra) 'foo) {:state :anything}))
(expect {:state :next :extra 'foo} ((side-effect (fn [state]
                                                   (merge state
                                                          {:state :next
                                                           :extra 'foo})))
                                    {:state :anything}))

;;; new-state returns a function that, when applied, will update the state machine's state
(given ((new-state :listening) {:state :initial :extra 'foo :state-alphabet #{:initial :listening}})
       (expect 
        :state :listening
        :extra 'foo))

(expect Exception ((new-state :listening) {:state-alphabet #{:one :two :three}}))


;;; on-event attaches a new transition to the transition map
(expect true (contains? (on-event {:input-alphabet #{:read :write}} :read identity) :transitions))
(expect true (seq? (get-in 
                    (on-event {:input-alphabet #{:read :write}} :read identity)
                    [:transitions :read])))
(expect false (seq? (get-in
                     (on-event {:input-alphabet #{:read :write}} :read identity)
                     [:transitions :write])))

(let [m1 {:input-alphabet #{:evt-a :evt-b :evt-c}
          :state-alphabet #{:waiting-for-a :waiting-for-b :waiting-for-c :waiting-for-godot}
          :state          :waiting-for-a}
      
      m1 (on-event m1 :evt-a
                   (in-state? :waiting-for-a)
                   (new-state :waiting-for-b))
      m1 (on-event m1 :evt-b
                   (in-state? :waiting-for-b)
                   (new-state :waiting-for-c))
      m1 (on-event m1 :evt-c
                   (in-state? :waiting-for-c)
                   (new-state :waiting-for-godot))
      m1 (on-event m1 :godot
                   (new-state :waiting-for-godot))]

  (expect :waiting-for-b (:state (evolve m1 :evt-a)))
  (expect :waiting-for-c (:state (evolve (evolve m1 :evt-a) :evt-b)))
  (expect :waiting-for-godot (:state (evolve (evolve (evolve m1 :evt-a) :evt-b) :evt-c)))
  (expect :waiting-for-godot (:state (evolve m1 :godot)))
  (expect :waiting-for-godot (:state (evolve (evolve (evolve m1 :godot) :godot) :godot)))
  (expect :waiting-for-godot (:state (evolve (assoc m1 :state :waiting-c) :godot)))
  
  )

;;; internal events for automatic state transitions
(let [m1 {:input-alphabet #{:evt-a :evt-b :evt-c}
          :state-alphabet #{:waiting-for-a :waiting-for-b :waiting-for-c :waiting-for-godot}
          :state          :waiting-for-a}

      m1 (on-event m1 :evt-a (in-state? :waiting-for-a) (generate-event :evt-b) (new-state :waiting-for-b))
      m1 (on-event m1 :evt-b (in-state? :waiting-for-b) (new-state :waiting-for-c) (generate-event :evt-c))
      m1 (on-event m1 :evt-c (in-state? :waiting-for-c) (new-state :waiting-for-godot))
      m1 (on-event m1 :godot (new-state :waiting-for-godot))]

  (expect :waiting-for-godot (:state (evolve m1 :evt-a)))
  
  )

(let [m2 {:input-alphabet #{:ping}
          :state-alphabet #{:receiving :sending}
          :state          :receiving}

      m2 (on-event m2 :ping     (in-state? :receiving) (new-state :sending))
      m2 (outputs  m2 :sending  (generate :pong)       (new-state :receiving))

      m2'  (evolve m2 :ping)
      m2'' (evolve m2' :ping)]

  (expect '(:pong) (:output m2'))
  (expect '(:pong :pong) (:output m2''))
  )

(let [m2 {:input-alphabet #{:ping}
          :state-alphabet #{:receiving :sending}
          :state          :receiving
          :condition      false}

      m2 (on-event m2 :ping     (in-state? :receiving)
                   (guard #(:condition %) (new-state :sending)))

      m2'  (evolve m2 :ping)

      m2'' (evolve (assoc m2 :condition true) :ping)]

  (expect :receiving (:state m2'))
  (expect :sending   (:state m2''))
  )

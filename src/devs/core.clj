(ns devs.core
  (:require [clojure.core.async :refer :all]
            [clojure.pprint :refer [pprint]]))

;; I could do this with a protocol and instances instead,
;; but then the resulting state machine would not be
;; open to inspection for debugging or automatic
;; documentation.
(defmulti do-transition (fn [a [b c]] b))

(defn new-state
  "Return a transition that moves the machine to the specified state."
  [to]
  [:new-state to])
(defmethod do-transition :new-state [m [_ to]] (-> m
                                                   (assoc :state to)
                                                   (update-in [:trace] conj to)))

(defn automatic
  "Return a transition that generates an automatic (internal) event."
  [in]
  [:automatic in])
(defmethod do-transition :automatic [m [_ in]] 
  (update-in m [:internal-events] conj in))

(defn- allowed-input?  [m in]  (some #{in}  (:input-alphabet m)))
(defn- allowed-output? [m out] (some #{out} (:output-alphabet m)))
(defn- allowed-state?  [m s]   (some #{s}   (:state-alphabet m)))

(defn on
"Adds to the state machine's transition function.
     in-state - the originating state
     input - an input symbol from the :input-alphabet
     state - new state to transition into (must exist in :states)

  The remaining arguments can include anything that implements ITransition,
  including guard clauses."
  [m in-state input to-state & txns]
  (let [move-it (new-state to-state)
        clauses (if txns (conj (vec txns) move-it) [move-it])]
    (assert (allowed-input? m input))
    (assert (allowed-state? m to-state))
    (assoc-in m [:transitions [in-state input]] clauses)))

(defn outputs
  "Adds to the state machine's output partial function.
     state - the machine's state
     symbols - a sequence of output symbols to emit _after_ the machine
               has completed a transition into that state."
  [m state symbol]
  (assert (allowed-state? m state))
  (assert (allowed-output? m symbol))
  (assoc-in m [:outputs state] symbol))

(defn guard
  [p]
  [:guard p])
(defmethod do-transition :guard [m [_ p]] (if (p m) m))

(defn- transition
  [machine input]
  (if-let [txns (get-in machine [:transitions [(:state machine) input]])]
    (do
      (last (take-while (comp not nil?)
                        (reductions #(do-transition %1 %2) machine txns))))))

(defn- evo-step
  [machine input]
  (let [next-state (update-in machine [:input-trace] conj input)
        next-state (transition next-state input)
        _          (when-not next-state (println "WARNING: transition rejected, reverting"))
        next-state (or next-state machine)
        out        (get-in next-state [:outputs (:state next-state)])
        deferred   (seq (:internal-events next-state))
        next-state (dissoc next-state :internal-events)]
    [next-state out deferred]))

(defn evolve
  "Evolve the state machine, given an input. Returns a new state machine as modified by
   the transition function and the output function. When automatic transitions are applied,
   evolve returns the final output. The intermediate states are not accessible."
  [machine input]
  (if-not (allowed-input? machine input)
    (throw (ex-info "Unrecognized input symbol" {:state-machine machine :input input}))
    (let [[ns out-v auto-v] (evo-step machine input)
          ns (if out-v (update-in ns [:output] concat [out-v]) ns)]
      (if-not auto-v
        ns
        (recur (assoc-in ns [:internal-events] (rest auto-v)) (first auto-v))))))

(defn evolve!
  "Start a state machine, with the given input channel. Every time a new
   input is presented, the state machine will 'tick' through it's evolution
   function. One input symbol may result in multiple state transitions and
   multiple output symbols, depending on automatic transitions.

   This function returns a pair of channels [output error]. The output channel
   will receive a series of values like [out-symbol machine-state].

   Unrecognized input symbols will 'break' the machine. In this case, it
   will emit a vector [:bad-input in-symbol machine-state] on the error channel.
   At that point, the machine will ignore any further input symbols.

   Exceptions in guard clauses will break the machine. It will emit a vector
   [:exception exception-obj machine-state] on the error channel."
  [machine in]
  (let [err (chan)
        out (chan)
        auto (chan)]
    (go
     (loop [state machine]
       (let [[input from-ch] (alts! [auto in] :priority true)]
         (if-not (allowed-input? state input)
           (>! err [:bad-input input state]))
         (let [[ns out-v auto-v] (evo-step state input)]
           (when auto-v
             (doseq [e auto-v]
               (put! auto e)))
           (when out-v
             (put! out [out-v ns]))
           (recur ns)))))
    [out err]))

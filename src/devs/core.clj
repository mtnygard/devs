(ns devs.core
  (:require [clojure.core.async :refer :all]
            [clojure.pprint :refer [pprint]]))

(defprotocol ITransition
  "Anything that can execute _during_ a state transition. This will be
   invoked after the input symbol and current state are recognized, but
   before the machine reflects the new state.

   To allow the transition, return the machine (possibly with alterations).
   To veto the transition, return nil."
  (on-transition [this m]))

(defn new-state
  "Return an ITransition that moves the machine to the specified state."
  [to]
  (reify ITransition
    (on-transition [this m] (assoc m :state to))))

(defn automatic
  "Return an ITransition that generates an automatic (internal) event."
  [in]
  (reify ITransition
    (on-transition [this m]
      (update-in m [:internal-events] conj in))))

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
    (assoc-in m [:transitions [in-state input]] clauses)) )

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
  (reify ITransition
    (on-transition [this m]
      (if (p m) m))))

(defn- transition
  [machine input]
  (if-let [txns (get-in machine [:transitions [(:state machine) input]])]
    (last (take-while (comp not nil?)
                      (reductions #(.on-transition %2 %1) machine txns)))))

(defn- evo-step
  [machine input]
  (let [next-state (or (transition machine input) machine)
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
         (let [[ns out-v auto-v] (evo-step machine input)]
           (println "evolve!" auto-v)
           (doseq [e auto-v] (>! auto e))
           (println "evolve!" out-v)
           (>! out [out-v state])
           (recur ns)))))
    [out err]))

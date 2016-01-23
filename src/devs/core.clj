(ns devs.core
  (:require [clojure.core.async :as a]))

(defmulti do-transition (fn [a [b c]] b))

(defn new-state
  "Return a transition that moves the machine to the specified state."
  [to]
  [:new-state to])

(defmethod do-transition :new-state [m [_ to]]
  (-> m
      (assoc :state to)
      (update :trace conj to)))

(defn automatic
  "Return a transition that generates an automatic (internal) event."
  [in]
  [:automatic in])

(defmethod do-transition :automatic [m [_ in]]
  (update m :internal-events conj in))

(defn- allowed-input?  [m in]  ((:input-alphabet  m) in))
(defn- allowed-output? [m out] ((:output-alphabet m) out))
(defn- allowed-state?  [m s]   ((:state-alphabet  m) s))

(defn- conjv [coll e] (conj (vec coll) e))

(defn on
"Adds to the state machine's transition function.
     in-state - the originating state
     input - an input symbol from the :input-alphabet
     state - new state to transition into (must exist in :states)

  The remaining arguments can include anything that implements ITransition,
  including guard clauses."
  [m in-state input to-state & txns]
  (let [next-state (new-state to-state)
        clauses    (conjv (or txns []) next-state)]
    (assert (allowed-input? m input))
    (assert (allowed-state? m to-state))
    (assoc-in m [:transitions [in-state input]] clauses)))

(defn outputs
  "Adds to the state machine's output partial function.
     state - the machine's state
     symbols - a sequence of output symbols to emit _after_ the machine
               has completed a transition into that state."
  [m state sym]
  (assert (allowed-state? m state))
  (assert (allowed-output? m sym))
  (assoc-in m [:outputs state] sym))

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

(defn- evolve-step
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
   the transition function and the output function. When automatic inputs are applied,
   evolve returns the final output. The intermediate states are not accessible."
  [machine input]
  (if-not (allowed-input? machine input)
    (throw (ex-info "Unrecognized input symbol" {:state-machine machine :input input}))
    (let [[next-state out-symbols auto-inputs] (evolve-step machine input)
          next-state                           (cond-> next-state
                                                 out-symbols
                                                 (update :output concat [out-symbols]))]
      (if-not auto-inputs
        next-state
        (recur (assoc next-state :internal-events (rest auto-inputs)) (first auto-inputs))))))

(defn evolve!
  "Start a state machine, with the given input channel. Every time a new
   input is presented, the state machine will 'tick' through it's evolution
   function. One input symbol may result in multiple state transitions and
   multiple output symbols, depending on automatic transitions.

   The output channel will receive a series of values like [out-symbol machine-state].

   Exceptions in guard clauses will break the machine. It will emit a vector
   [:exception exception-obj machine-state] on the error channel.

   Unrecognized input symbols will 'break' the machine. In this case, it
   will emit a vector [:bad-input in-symbol machine-state] on the error channel.
   At that point, the machine will ignore any further input symbols.

   This function returns a channel that will close when the machine terminates.

   If the input channel is closed, the machine will terminate."
  [machine in out err]
  (let [auto (a/chan (a/dropping-buffer 1000))]
    (a/go-loop [state machine]
      (let [[input from-ch] (a/alts! [auto in] :priority true)]
        (when input
          (if-not (allowed-input? state input)
            (a/>! err [:bad-input input state]))
          (let [[next-state out-symbols auto-inputs] (evolve-step state input)]
            (doseq [e auto-inputs]
              (a/put! auto e))
            (when out-symbols
              (a/put! out [out-symbols next-state]))
            (recur next-state)))))))

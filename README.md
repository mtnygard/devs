# DEVS - Discrete Event System Specification

This library provides a state machine as data. There is a single,
generic "evolve" function that takes the state machine and an input,
and produces a new state machine.

Formally, the state machine is specified as a tuple of the following:

   * S   - The state alphabet
   * s   - The current state
   * A_i - The input alphabet
   * A_o - The output alphabet
   * F   - The transition function from (s, A_i) -> n, where n is the next state
   * O   - The output function from (C_n+1, A_o) -> o, where o is a symbol from A_o

An easy way to think of this is that the state machine accepts any
symbol in the input and "transitions" to a new state based on the
current state and that input symbol. As it "transitions," it produces
an output from the output alphabet.

In implementation terms, the state machine at any point is
immutable. Evolving the state machine means returning a new instance
derived from the old one.

This means that you can actually change the transition function when
the state machine moves to new states. You can also change the input
or output alphabets.

## Describing the state machine

Here is an example from an "echo" server:

    (defn echo-server-state-machine
      "Construct a DEVS data structure for the echo server protocol."
      [socket]
      (-> {:input-alphabet         #{:read :write :empty :close :empty-buffers}
           :output-alphabet        #{:read :write}
           :state-alphabet         #{:reading :writing :draining :closed}
           :state                  :reading
           :socket                 socket}
          (on :read
              (in-state? :reading)
              (new-state :writing))
          (on :read
              (in-state? :writing)
              (new-state :writing))
          (on :write
              (in-state? :writing)
              (new-state :writing))
          (on :write
              (in-state? :draining)
              (guard all-data-drained?
                     (generate-event :empty-buffers))
              (new-state :draining))
          (on :empty-buffers
              (in-state? :writing)
              (new-state :reading))
          (on :empty-buffers
              (in-state? :draining)
              (new-state :closed))
          (on :close
              (in-state? :reading)
              (new-state :closed))
          (on :close
              (in-state? :writing)
              (new-state :draining))
          (outputs :reading  (generate [:read]))
          (outputs :writing  (generate [:read :write]))
          (outputs :draining (generate [:write]))
          (outputs :closed   (generate []))))

This code builds a state machine definition by passing maps in to various
high-level "builder" functions like `on`. `on` builds up the transition function
as a relation over the cross product of S and A_i. This allows a much more
compact representation than writing the total function F!

Likewise, `outputs` is a builder function for the output function. It
accumulates states and "generators" for the output tape.

## Evolving the state machine

No matter what your alphabets or functions, there is just one function
to evolve from one state to the next:

    (defn evolve [state-machine input-symbol])

It returns a new state machine. Keep it in an Agent, an Atom, or just
a locally bound variable. For example, to process a (hopefully finite)
seq of inputs:

    (reduce evolve state-machine inputs)

Then when you're done, you can examine the output tape, which is bound
to the key `:output`.

## More Info

For more detail about Atomic DEVS, see http://en.wikipedia.org/wiki/DEVS#Atomic_DEVS.

A couple of nuances about this library's implementation:

   * It implements the reactive portion of an Atomic DEVS state
      machine, but does not handle internal transitions triggered by
      "collapse" of states from their lifespans elapsing. (Coming soon!)
   * It allows a series of automatic state transitions to happen
      atomically. Each automatic transition can generate an output
      symbol. Thus a single input symbol may result in a series of outputs.

## License

Copyright © 2013-2018 Michael T. Nygard

Distributed under the Eclipse Public License, the same as Clojure.

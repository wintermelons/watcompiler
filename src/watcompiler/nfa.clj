(ns watcompiler.nfa
  (:require [clojure.set :refer :all]
            [watcompiler.dfa :refer :all])
  (:import [watcompiler.dfa DFA]))

;; Define epsilon (e)
;; do not directly use -epsilon-t, use epsilon instead
(defrecord -epsilon-t []
  Object (toString [_] "ε"))
;; Singleton
(def epsilon (-epsilon-t.))
(def e epsilon)
;; Predicates
(def epsilon? (partial instance? -epsilon-t))
(def e? epsilon?)

;; Epsilon-NFA definition
(defrecord NFA
  [alphabet states start accept-states transitions])

(defn e-closure
  "Returns the epsilon closure of a set of states"
  [nfa states]
  (let [trans (:transitions nfa)
        lookup (partial get trans)]
    (loop [closure (set states)
           queue (vec states)]
      (if (empty? queue)
        closure
        (let [state (first queue)
              neighbors (lookup (list state e))]
          (recur (union closure neighbors)
                 (into (rest queue) (difference neighbors closure))))))))

(defn accept?
  "Tests if states contains an accepting state, and if it does, returns the kind"
  [nfa states]
  (let [final-states (intersection states (set (keys (:accept-states nfa))))
        state-to-kind (reduce-kv (fn [m k [v _]] (assoc m k v)) {} (:accept-states nfa))
        state-to-priority (reduce-kv (fn [m k [_ v]] (assoc m k v)) {} (:accept-states nfa))]
    (if (empty? final-states)
      false
      (let [state-priorities (select-keys state-to-priority final-states)
            min-state (key (apply min-key val state-priorities))]
        (state-to-kind min-state)))))

(defn make-transition-NFA
  [transitions]
  (let [add-to-map (fn [m k v]
                     (if (contains? m k)
                       (assoc m k (conj (m k) v))
                       (assoc m k #{v})))]
  (loop [remaining transitions
         transition-map {}]
    (if (empty? remaining)
      transition-map
      (let [[s-from s-to alphabets] (first remaining)]
        (recur (rest remaining)
               (if (seq? alphabets)
                 (reduce #(add-to-map %1 (list s-from %2) s-to)
                         transition-map
                         alphabets)
                 (add-to-map transition-map (list s-from alphabets) s-to))))))))

(defn run-NFA
  "Runs NFA if with input, returns true if accepted"
  [nfa input]
  (let [trans (:transitions nfa)
        lookup (partial get trans)]
    (loop [chars (char-array input)
           states #{(:start nfa)}]
      (if (empty? chars)
        ;; Test if accepting
        ;; Read next char
        (accept? nfa states)
        (let [[letter & rest-chars] chars
              closure (e-closure nfa states)
              next-states (map #(lookup (list % letter)) closure)
              new-states (apply union next-states)]
          (if (empty? new-states)
            false
            (recur rest-chars
                   new-states)))))))

(defn NFA-to-DFA
  "Builds an equivalent DFA from a NFA"
  [nfa]
  (let [neighbor-nodes (reduce-kv (fn [final [state letter] _]
                                    (if (contains? final state)
                                      (assoc final state (conj (final state) letter))
                                      (assoc final state #{letter})))
                                  {}
                                  (:transitions nfa))
        lookup (partial get (:transitions nfa))
        take-transition (fn [states letter]
                          (apply union (map #(lookup (list % letter)) states)))
        dfa-start (e-closure nfa #{(:start nfa)})
        dfa-start-kind (accept? nfa dfa-start)]
    (loop [dfa-states #{dfa-start}
           dfa-accept-states (if dfa-start-kind
                               {dfa-start dfa-start-kind}
                               {})
           dfa-transitions {}
           queue [dfa-start]]
      (if (empty? queue)
        ;; We're done, build the DFA
        (DFA. (:alphabet nfa)
              dfa-states
              dfa-start
              dfa-accept-states
              dfa-transitions)
        (let [states (first queue)
              possible-letters (distinct (filter #(not (e? %)) 
                                                 (apply union (map #(neighbor-nodes %) states))))
              next-states (map (partial e-closure nfa) 
                               (map (partial take-transition states) possible-letters))
              ;; add each next-states to states and queue, and transition function
              handle (fn []
                       (loop [dfa-states dfa-states
                              dfa-accept-states dfa-accept-states
                              dfa-transitions dfa-transitions
                              outer-queue (rest queue)
                              states-queue next-states
                              letters-queue possible-letters]
                         (if (empty? states-queue)  ;; letters-queue should be empty too
                           (list dfa-states
                                 dfa-accept-states
                                 dfa-transitions
                                 outer-queue)
                           (let [next-state (first states-queue)
                                 next-letter (first letters-queue)
                                 next-transitions (assoc dfa-transitions (list states next-letter) next-state)]
                             (if (contains? dfa-states next-state)
                               (recur dfa-states
                                      dfa-accept-states
                                      next-transitions
                                      outer-queue
                                      (rest states-queue)
                                      (rest letters-queue))
                               (let [accepted-kind (accept? nfa next-state)]
                                 (if accepted-kind
                                   (recur (conj dfa-states next-state)
                                          (assoc dfa-accept-states next-state accepted-kind)
                                          next-transitions
                                          (conj outer-queue next-state)
                                          (rest states-queue)
                                          (rest letters-queue))
                                   (recur (conj dfa-states next-state)
                                          dfa-accept-states
                                          next-transitions
                                          (conj outer-queue next-state)
                                          (rest states-queue)
                                          (rest letters-queue)))))))))
              [next-dfa-states next-dfa-accept-states next-dfa-transitions next-queue] (handle)]
          (recur next-dfa-states
                 next-dfa-accept-states
                 next-dfa-transitions
                 next-queue))))))

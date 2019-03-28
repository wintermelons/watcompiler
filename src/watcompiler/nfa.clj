(ns watcompiler.nfa
  (:require [clojure.set :refer :all]
            [watcompiler.dfa :refer :all]))

;; Define epsilon (e)
;; do not directly use -epsilon-t, use epsilon instead
(defrecord -epsilon-t []
  Object (toString [_] "ε"))

;; Singleton
(def epsilon
  (-epsilon-t.))
(def e epsilon)

;; Predicates
(def epsilon?
  (partial instance? -epsilon-t))
(def e? epsilon?)

;; ε-NFA definition
;; See watcompiler.lang/simple-nfa for an example.
;; An ε-NFA is a 6-tuple of
;;   alphabet:          set of letters; must be characters
;;   states:            set of states; can be any type, even sets
;;   start:             the start state; must be in states
;;   accept-states:     hashmap state => kind
;;   transitions:       hashmap (state, letter or ε) => set of states
;;   accept-priorities: hashmap state => priority; priority must be numerical
(defrecord NFA
  [alphabet states start accept-states transitions accept-priorities])

;; Takes in accept-map as hashmap of state => (kind, priority)
(defn make-NFA
  "Constructor for an NFA"
  [alphabet states start accept-map transitions]
  ;; TODO: Add validations
  (let [accept-states (reduce-kv (fn [m k [v _]] (assoc m k v)) {} accept-map)
        accept-priorities (reduce-kv (fn [m k [_ v]] (assoc m k v)) {} accept-map)]
    (->NFA alphabet
           states
           start
           accept-states
           transitions
           accept-priorities)))

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
  (let [final-states (intersection states (set (keys (:accept-states nfa))))]
    (if (empty? final-states)
      false
      (let [state-priorities (select-keys (:accept-priorities nfa) final-states)
            min-state (key (apply min-key val state-priorities))]
        ((:accept-states nfa) min-state)))))

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
               (if (or (seq? alphabets) (vector? alphabets))
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

;; Alphabet remains unchanged
;; Each state in the DFA is a ε-closure set of states in the NFA
;; Each state in the DFA has all possible transitions by each of the states
;;   in the NFA, each to their corresponding ε-closure.
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
    ;; outer loop
    (loop [dfa-states #{dfa-start}
           dfa-accept-states (if dfa-start-kind
                               {dfa-start dfa-start-kind}
                               {})
           dfa-transitions {}
           queue [dfa-start]]
      (if (empty? queue)
        ;; We're done, build the DFA
        (make-DFA (:alphabet nfa)
                  dfa-states
                  dfa-start
                  dfa-accept-states
                  dfa-transitions)
        (let [states (first queue)
              possible-letters (distinct (filter #(not (e? %))
                                                 (apply union (map #(neighbor-nodes %) states))))
              next-states (map (partial e-closure nfa)
                               (map (partial take-transition states) possible-letters))
              ;; handle function to add each next-states to states and queue, and transition function
              ;; need inner loop for this, we wrap this into a function
              handle (fn []
                       (loop [dfa-states dfa-states
                              dfa-accept-states dfa-accept-states
                              dfa-transitions dfa-transitions
                              outer-queue ()
                              states-queue next-states
                              letters-queue possible-letters]
                         (if (empty? states-queue)  ;; letters-queue should be empty too
                           ;; We are done, return updated states, accept states, transitions and new elements to outer queue
                           (list dfa-states
                                 dfa-accept-states
                                 dfa-transitions
                                 outer-queue)
                           ;; There are more neighbors to process
                           (let [next-state (first states-queue)
                                 next-letter (first letters-queue)
                                 next-transitions (assoc dfa-transitions (list states next-letter) next-state)]
                             (if (contains? dfa-states next-state)
                               ;; state already known, no need to process it iagain
                               ;; in any case we must add it to our transitions function
                               (recur dfa-states
                                      dfa-accept-states
                                      next-transitions
                                      outer-queue
                                      (rest states-queue)
                                      (rest letters-queue))
                               ;; state unknown, we check if it is accepting
                               (let [accepted-kind (accept? nfa next-state)]
                                 (if accepted-kind
                                   ;; new state is accepting, so we add it to dfa-accepting-states
                                   (recur (conj dfa-states next-state)
                                          (assoc dfa-accept-states next-state accepted-kind)
                                          next-transitions
                                          (conj outer-queue next-state)
                                          (rest states-queue)
                                          (rest letters-queue))
                                   ;; new state not accepting
                                   (recur (conj dfa-states next-state)
                                          dfa-accept-states
                                          next-transitions
                                          (conj outer-queue next-state)
                                          (rest states-queue)
                                          (rest letters-queue)))))))))
              ;; destructure here to feed into recur
              [next-dfa-states next-dfa-accept-states next-dfa-transitions next-queue] (handle)]
          (recur next-dfa-states
                 next-dfa-accept-states
                 next-dfa-transitions
                 (into (rest queue) next-queue)))))))

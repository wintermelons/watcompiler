(ns watcompiler.nfa
  (:require [clojure.set :refer :all]))

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
        (let [final-states (intersection states (set (keys (:accept-states nfa))))
              state-to-kind (reduce-kv (fn [m k [v _]] (assoc m k v)) {} (:accept-states nfa))
              state-to-priority (reduce-kv (fn [m k [_ v]] (assoc m k v)) {} (:accept-states nfa))]
          (if (empty? final-states)
            false
            (let [state-priorities (select-keys state-to-priority final-states)
                  min-state (key (apply min-key val state-priorities))]
              (state-to-kind min-state))))
        ;; Read next char
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
        dfa-start (e-closure nfa #{(:start nfa)})]
    (loop [dfa-states #{}
           dfa-accept-states #{}
           dfa-transitions {}
           curr-states dfa-start]
      (if ???
        nil
        nil)))

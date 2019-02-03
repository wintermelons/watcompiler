(ns watcompiler.cfg
  (:require [clojure.set :refer :all]))

;; Context-Free Grammar definition
;; See watcompiler.lang/simple-cfg for an example.
;; A CFG is a 4-tuple of
;;   terminals:     set of letters; must be characters
;;   non-terminals: set of symbols; can be any type, must be disjoint from terminals
;;   start:         symbol to start
;;   rules:         vector of (symbol (symbol ...))
(defrecord CFG
  [terminals non-terminals start rules])

(defn make-CFG
  "Constructor for CFG"
  [terminals non-terminals start rules]
  (->CFG terminals non-terminals start rules))

(defn leftmost
  "Follow a list of leftmost derivations to obtain a final word
  returns produced word, and the number of steps taken."
  [cfg derivations]
  (loop [produced []
         buffer (list (:start cfg))
         derivations derivations
         steps 0]
    (if (empty? derivations)
      ;; we are done
      (list (into produced buffer) steps)
      ;; derive the next non-terminal
      (if (empty? buffer)
        ;; nothing left to derive
        (list produced steps)
        (if (contains? (:terminals cfg) (first buffer))
          ;; shift char to produced
          (recur (conj produced (first buffer))
                 (rest buffer)
                 derivations
                 steps)
          ;; derive using rule
          (let [[from to] (nth (:rules cfg) (first derivations))]
            (if (not (= from (first buffer)))
              ;; derivation does not match first non-terminal
              (list (into produced buffer) steps)
              (recur produced
                     (concat to (rest buffer))
                     (rest derivations)
                     (inc steps)))))))))

(defn rightmost
  "Follow a list of rightmost derivations to obtain a final word"
  [cfg derivations]
  (loop [produced ()
         buffer (list (:start cfg))
         derivations derivations
         steps 0]
    (if (empty? derivations)
      ;; we are done
      (list (vec (concat (reverse buffer) produced)) steps)
      ;; derive the next non-terminal
      (if (empty? buffer)
        ;; nothing left to derive
        (list (vec produced) steps)
        (if (contains? (:terminals cfg) (first buffer))
          ;; shift char to produced
          (recur (cons (first buffer) produced)
                 (rest buffer)
                 derivations
                 steps)
          ;; derive using rule
          (let [[from to] (nth (:rules cfg) (first derivations))]
            (if (not (= from (first buffer)))
              ;; derivation does not match first non-terminal
              (list (vec (concat (reverse buffer) produced)) steps)
              (recur produced
                     (concat (reverse to) (rest buffer))
                     (rest derivations)
                     (inc steps)))))))))



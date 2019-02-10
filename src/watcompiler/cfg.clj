(ns watcompiler.cfg
  (:require [clojure.set :refer :all]))

;; Context-Free Grammar definition
;; See watcompiler.lang/simple-cfg for an example.
;; A CFG is a 4-tuple of
;;   terminals:     set of letters; must be characters
;;   non-terminals: set of symbols; can be any type, must be disjoint from terminals
;;   start:         symbol to start
;;   productions:   vector of (symbol (symbol ...))
(defrecord CFG
  [terminals non-terminals start productions])

(defn make-CFG
  "Constructor for CFG"
  [terminals non-terminals start productions]
  (->CFG terminals non-terminals start productions))

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
          (let [[from to] (nth (:productions cfg) (first derivations))]
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
          (let [[from to] (nth (:productions cfg) (first derivations))]
            (if (not (= from (first buffer)))
              ;; derivation does not match first non-terminal
              (list (vec (concat (reverse buffer) produced)) steps)
              (recur produced
                     (concat (reverse to) (rest buffer))
                     (rest derivations)
                     (inc steps)))))))))

(defn rightmost-tree
  "Builds a parse tree from a list of derivations; builds top-down"
  [cfg derivations]
  (loop [access-ks [0]
         tree [(:start cfg)]
         derivations derivations]
    (if (or (empty? derivations)  ;; no more derivations
            (= access-ks [-1]))   ;; end of the tree
      ;; we are done
      (first tree)
      (let [curr-sym (get-in tree access-ks)]
        (if (nil? curr-sym)
          ;; reached end of subtree
          (let [parent (pop (pop access-ks))]
            (recur (conj (pop parent) (dec (peek parent)))
                   tree
                   derivations))
          (if (vector? curr-sym)
            ;; need to recurse into subtree
            (recur (conj access-ks (dec (count curr-sym)))
                   tree
                   derivations)
            ;; we are at leaf
            (if (contains? (:terminals cfg) curr-sym)
              ;; terminal
              (recur (conj (pop access-ks) (dec (peek access-ks)))
                     tree
                     derivations)
              ;; we see a non-terminal
              (let [[from to] (nth (:productions cfg) (first derivations))]
                (if (= from curr-sym)
                  (recur (conj access-ks 1 (dec (count to)))
                         (assoc-in tree access-ks [from to])
                         (rest derivations))
                  nil)))))))))

(defn lr0
  "Builds a LR(0) DFA using a list of production rules"
  [cfg]
  42)

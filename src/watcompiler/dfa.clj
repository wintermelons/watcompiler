(ns watcompiler.dfa)

(defrecord DFA
  [alphabet states start accept-states transitions])

(defn make-transition
  [transitions]
  (loop [remaining transitions
         transition-map {}]
    (if (empty? remaining)
      transition-map
      (let [[s-from s-to alphabets] (first remaining)]
        (recur (rest remaining)
               (if (vector? alphabets)
                 (reduce #(assoc %1 (list s-from %2) s-to)
                         transition-map
                         alphabets)
                 (assoc transition-map 
                        (list s-from alphabets) 
                        s-to)))))))

(defn run-DFA 
  "Runs dfa with input, returns true if accepted"
  [dfa input]
  (loop [chars (char-array input)
         state (:start dfa)]
    (if (empty? chars)
      (contains? (:accept-states dfa) state)
      (let [[letter & rest-chars] chars]
        (if (contains? (:alphabet dfa) letter)
          (let [desc (list state letter)]
            (if (contains? (:transitions dfa) desc)
              (recur rest-chars
                     (get (:transitions dfa) desc))
              false))
          (do 
            (binding [*out* *err*]
              (println "Warning: Letter" letter "is not in alphabet"))
            false))))))

(defn munch-DFA
  "Runs dfa with input, returns longest valid word, its length and its kind"
  [dfa input]
  (loop [chars input
         state (:start dfa)
         curr-word []
         curr-length 0
         valid-word []
         valid-length 0
         valid-kind nil]
    (if (empty? chars)
      ;; we are done
      (list (clojure.string/join valid-word) valid-length valid-kind)
      ;; (contains? (:accept-states dfa) state)
      (let [[letter & rest-chars] chars
            new-word (conj curr-word letter)
            new-length (inc curr-length)]
        (let [desc (list state letter)]
          (if-not (contains? (:transitions dfa) desc) 
            ;; crashed
            (list (clojure.string/join valid-word) valid-length valid-kind)
            (let [new-state (get (:transitions dfa) desc)]
              (if (contains? (:accept-states dfa) new-state)
                ;; update valid-word, valid-length and valid-kind
                (recur rest-chars
                       new-state
                       new-word
                       new-length
                       new-word
                       new-length
                       (get (:accept-states dfa) new-state))
                ;; keep the same
                (recur rest-chars
                       new-state
                       new-word
                       new-length
                       valid-word
                       valid-length
                       valid-kind)))))))))

(defn scan-DFA
  "Scans input with dfa by Maximal Munch, returns list of tokens and their kind"
  [dfa input]
  (loop [remaining input
         tokens []]
    (if (empty? remaining) 
      tokens
      (let [[word length kind] (munch-DFA dfa remaining)]
        (if (= length 0)
          (binding [*out* *err*] 
            (println "Invalid program! Scanning error found at:")
            (println (clojure.string/join (take 20 remaining))))
          (recur (drop length remaining)
                 (conj tokens (list (clojure.string/join word) kind))))))))

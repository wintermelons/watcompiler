(ns watcompiler.core
  (:require [watcompiler.dfa :refer :all]
            [watcompiler.nfa :refer :all]
            [watcompiler.lang :refer :all]
            [watcompiler.cfg :refer :all])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (doseq [file args]
    (let [program (slurp file)]
      (println "Scanning file:" file)
      (println "--- source ---")
      (println program)
      (println "--- tokens ---")
      (println (scan-DFA simple-dfa program)))))

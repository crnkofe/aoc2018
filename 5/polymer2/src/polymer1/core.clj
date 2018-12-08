(ns polymer1.core
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:gen-class))

(defn combo-set [] 
  (let [small "abcdefghijklmnopqrstuvwxyz"
        large "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
    (set/union (set (map #(str (nth small %) (nth large %)) (range (count small))))
           (set (map #(str (nth large %) (nth small %)) (range (count small)))))
  ))

(defn individual-combo-set [] 
  (let [small "abcdefghijklmnopqrstuvwxyz"
        large "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        pairs (map #(vector (str (nth small %)) (str (nth large %))) (range (count small)))]
    (map #(set %) pairs)
  ))

(defn delete-combos [input combo-data]
  (loop [current-input input
         combos (into [] combo-data)]
    (if (empty? combos)
      current-input
      (recur (str/replace current-input (first combos) "") (rest combos))
  )))

(defn delete-all-occurrences [input combo-data]
  (loop [current-input input]
    (let [new-input (delete-combos current-input combo-data)]
        (if (= (count current-input) (count new-input))
          current-input
          (recur new-input))
      )
    )
  )


(defn delete-prepare [input combo-data individual-combo-data]
  (loop [reductions {}
         individual-combos (into [] individual-combo-data)]
    (if (empty? individual-combos)
      reductions
      (let [current-combo (set (first individual-combos))
            prepared-input (delete-all-occurrences input current-combo)]
        (recur (assoc reductions (first individual-combos) (- (count (delete-all-occurrences prepared-input combo-data)) 1)) (rest individual-combos))
        )
  )))


(defn -main
  [& args]
  (let [data (slurp (nth args 0))
        combos (combo-set)
        individual-combos (individual-combo-set)
        all-possible-removals (first (sort-by #(nth % 1) (delete-prepare data combos individual-combos)))
        ]
    (println all-possible-removals)
    (println (last all-possible-removals))
  ))

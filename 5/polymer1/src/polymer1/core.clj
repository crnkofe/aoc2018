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

(defn -main
  [& args]
  (let [data (slurp (nth args 0))
          combos (combo-set)]
      (println (- (count (delete-all-occurrences data combos)) 1))
  ))

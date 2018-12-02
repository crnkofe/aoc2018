(ns checksum2.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo])
  )

(defn check-sum [w1 w2]
  (let [wsz (count w1)
        differences (map #(= (nth w1 %) (nth w2 %)) (range wsz))
        zipped (map vector w1 differences)
        remaining (filter #(= true (nth % 1)) zipped)]
    (apply str (map #(nth % 0) remaining))
  ))

(defn one-compare [w1 w2]
  (let [wsz (count w1)
        differences (map #(= (nth w1 %) (nth w2 %)) (range wsz))]
    (count (filter #(= false %) differences))
  ))

(defn find-mismatch [words]
  (let [word-size (count words)
        cart-words (filter #(not= (nth % 0) (nth % 1)) (combo/cartesian-product words words))
        result (first (filter #(= 1 (one-compare (nth % 0) (nth % 1))) cart-words))]
    (check-sum (nth result 0) (nth result 1))
  ))

(defn -main
    [& args]
      (with-open [rdr (clojure.java.io/reader (nth args 0))]
            (println (find-mismatch (line-seq rdr)))
                ))

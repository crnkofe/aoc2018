(ns safety1.core
  (:require [clojure.string :as str])
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn limits [points]
  (let [x (apply min (map #(first %) (vals points)))
        y (apply min (map #(last %) (vals points)))
        max-x (apply max (map #(first %) (vals points)))
        max-y (apply max (map #(last %) (vals points)))]
  (vector (vector x y) (vector max-x max-y))))

(defn generate-hash-table [upper-left bottom-right]
  (let [[x y] upper-left
        [mx my] bottom-right
        points (combo/cartesian-product (range x (+ 1 mx)) (range y (+ 1 my)))]
    (zipmap points (repeat (count points) -1 ))
  ))

(defn calc-manhattan-distance [point origin]
  (let [[x y] point
        [xo yo] origin]
    (+ (Math/abs (- xo x)) (Math/abs (- yo y)))
  ))

(defn find-closest-origin [point origins]
  (let [all-distances (map #(vector % (calc-manhattan-distance point %)) origins)
        min-distance (apply min (map #(last %) all-distances))
        closest-origins (filter #(= (last %) min-distance) all-distances)]
    (if (> (count closest-origins) 1)
      nil
      (first (first closest-origins)))
  ))

(defn paint-table [table points]
  (loop [current-table table
         current-points (keys table)]
    (if (empty? current-points)
      current-table
      (let [closest-origin (find-closest-origin (first current-points) (vals points))]
        (if (nil? closest-origin)
          (recur current-table (rest current-points))
          (let [found (first (some #(if (= closest-origin (last %)) %) points))]
            (recur (assoc current-table (first current-points) found)  (rest current-points))
            )
          )
        )
      )
    )
  )


(defn total-manhattan [points point]
  (reduce + (map #(calc-manhattan-distance point %) points))
  )

(defn manhattanize [points top-left bottom-right range-check max-manhattan-sum]
  (let [[x y] top-left
        [mx my] bottom-right
        extended-points (combo/cartesian-product (range (- x range-check) (+ range-check mx)) (range (- y range-check) (+ range-check my)))]
    (count (filter #(< (total-manhattan (vals points) %) max-manhattan-sum) extended-points))
    )
  )

(defn border-points [table upper-left bottom-right] 
  (let [[x y] upper-left
        [xo yo] bottom-right
        x-set (vals (filter #(= x (first (first %))) table))
        y-set (vals (filter #(= y (last (first %))) table))
        xo-set (vals (filter #(= xo (first (first %))) table))
        yo-set (vals (filter #(= yo (last (first %))) table))
        all (distinct (flatten (conj x-set y-set xo-set yo-set)))]
    (into #{} (filter #(not= -1 %) all))
    )
  )

(defn -main
  [& args]
    (with-open [rdr (clojure.java.io/reader (nth args 0))]
      (let [manhattan-sum (Integer. (nth args 1))
            range-check (Integer.  (nth args 2))
            points (into {} (map-indexed #(vector %1 (map read-string (str/split %2 #", "))) (line-seq rdr)))
            borders (limits points)]
        (println (manhattanize points (first borders) (last borders) range-check manhattan-sum))
        ; (println (apply max (vals (frequencies (vals filtered-infinite-table)))))
       )
  ))

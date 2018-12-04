(ns overlap1.core
  (:gen-class)
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pprint])
  (:require [clojure.math.combinatorics :as combo])
  )

(defn find-all-ones [matrix box]
  (let [split-str (str/split box #" ")
        x (read-string (nth (str/split (nth split-str 2) #",") 0))
        y (read-string (str/join "" (drop-last (nth (str/split (nth split-str 2) #",") 1))))
        w (read-string (nth (str/split (nth split-str 3) #"x") 0))
        h (read-string (nth (str/split (nth split-str 3) #"x") 1))]
  (loop [pixels (combo/cartesian-product (range w) (range h))]
      (if (empty? pixels)
       true
       (let [nx (+ x (nth (first pixels) 0))
             ny (+ y (nth (first pixels) 1))]
         (if (not= 1 (nth (nth matrix ny) nx)) 
           false
           (recur (rest pixels)))
       )))))

(defn generate-matrix [size]
  (vec (repeat size (vec (repeat size 0))))
  )

(defn paint-matrix [matrix x y]
  (assoc matrix y (assoc (nth matrix y) x (+ (nth (nth matrix y) x) 1)))
  )

(defn draw [matrix box]
  (let [split-str (str/split box #" ")
        x (read-string (nth (str/split (nth split-str 2) #",") 0))
        y (read-string (str/join "" (drop-last (nth (str/split (nth split-str 2) #",") 1))))
        w (read-string (nth (str/split (nth split-str 3) #"x") 0))
        h (read-string (nth (str/split (nth split-str 3) #"x") 1))]
  (loop [converted-matrix matrix
         pixels (combo/cartesian-product (range w) (range h))]
      (if (empty? pixels)
        converted-matrix
       (let [nx (+ x (nth (first pixels) 0))
             ny (+ y (nth (first pixels) 1))]
        (recur (paint-matrix converted-matrix nx ny) (rest pixels))
       )))))

(defn draw-all [matrix boxes]
  (loop [converted-matrix matrix
         current-boxes boxes]
      (if (empty? current-boxes)
        converted-matrix
        (recur (draw converted-matrix (first current-boxes)) (rest current-boxes)
       )))
  )

(defn count-occupied [matrix min-num]
  (let [filtered-matrix (filter #(>= % min-num) (flatten matrix))]
      (count filtered-matrix)))

(defn -main
  [& args]
  (with-open [rdr (clojure.java.io/reader (nth args 0))
              rdr1 (clojure.java.io/reader (nth args 0))]
    (let [result (draw-all (generate-matrix 1000) (line-seq rdr) )]
      (println (filter #(find-all-ones result %) (line-seq rdr1)))
  )))

